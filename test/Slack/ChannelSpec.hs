{-# LANGUAGE QuasiQuotes #-}

module Slack.ChannelSpec (spec) where

import Control.Lens ((&), (^.))
import Data.IORef (modifyIORef, newIORef, readIORef)
import NeatInterpolation (trimming)
import Network.Wreq (params)
import Polysemy.Log (LogMessage (..), Severity (..))
import Slack.Channel
  ( Channel (..),
    Effects,
    runChannels,
  )
import Slack.Channel qualified as Channels (create, find, setTopic)
import Slack.TestUtils
  ( SlackResponse (..),
    nullSlackMatch,
    runSlackWith,
    runSlackWithExpectations,
  )
import Slack.Util (Slack (..))
import Test.Hspec
  ( Spec,
    context,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
  )

spec :: Spec
spec = do
  describe "find" $ do
    it "queries conversations.list" $ do
      Channels.find "name"
        & runWithExpectations \case
          GetPaginated _ method -> method `shouldBe` "conversations.list"
          _ -> expectationFailure "Expected a GetPaginated query"

    it "sets response limit to 1000" $ do
      Channels.find "name"
        & runWithExpectations \case
          GetPaginated opts _ -> opts ^. params `shouldContain` [("limit", "1000")]
          _ -> expectationFailure "Expected a GetPaginated query"

    it "excludes archived channels" $ do
      Channels.find "name"
        & runWithExpectations \case
          GetPaginated opts _ -> opts ^. params `shouldContain` [("exclude_archived", "true")]
          _ -> expectationFailure "Expected a GetPaginated query"

    it "includes public and private channels" $ do
      Channels.find "name"
        & runWithExpectations \case
          GetPaginated opts _ -> opts ^. params `shouldContain` [("types", "public_channel,private_channel")]
          _ -> expectationFailure "Expected a GetPaginated query"

    it "fails on a response of an empty object" $ do
      Channels.find "name"
        & runGetPaginatedConst ["{}"]
        & snd -- Ignore logs
        & (`shouldBe` Left "\"conversations.list\" response didn't include a \"channels\" field")

    it "retuns Nothing if the list of channels is empty" $ do
      Channels.find "name"
        & runGetPaginatedConst ["{\"channels\": []}"]
        & snd -- Ignore logs
        & (`shouldBe` Right Nothing)

    it "fails if channel object does not have an 'id' key" $ do
      Channels.find "name"
        & runGetPaginatedConst ["{\"channels\": [{}]}"]
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"id\" not found")

    it "fails if channel object does not have a 'name' key" $ do
      Channels.find "name"
        & runGetPaginatedConst ["{\"channels\": [{\"id\": \"something\"}]}"]
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"name\" not found")

    it "fails if channel object does not have a 'topic' key" $ do
      Channels.find "name"
        & runGetPaginatedConst
          [ [trimming|
            {
              "channels": [{
                "id": "id",
                "name": "name"
              }]
            }
            |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"topic\" not found")

    it "fails if 'topic' key is not an object" $ do
      Channels.find "name"
        & runGetPaginatedConst
          [ [trimming|
            {
              "channels": [{
                "id": "id",
                "name": "name",
                "topic": "topic"
              }]
            }
          |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Left "parsing KeyMap failed, expected Object, but encountered String")

    it "fails if topic object does not have a 'value' key" $ do
      Channels.find "name"
        & runGetPaginatedConst
          [ [trimming|
            {
              "channels": [{
                "id": "id",
                "name": "name",
                "topic": {}
              }]
            }
          |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"value\" not found")

    it "returns nothing if the channels in the response don't match the name" $ do
      Channels.find "seatch_name"
        & runGetPaginatedConst
          [ [trimming|
            {
              "channels": [{
                "id": "id",
                "name": "other_name",
                "topic": {
                  "value": "topic"
                }
              }]
            }
          |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Right Nothing)

    it "finds channel with a matching name" $ do
      Channels.find "name"
        & runGetPaginatedConst
          [ [trimming|
            {
              "channels": [{
                "id": "id",
                "name": "name",
                "topic": {
                  "value": "topic"
                }
              }]
            }
          |]
          ]
        & ( `shouldBe`
              ( [ LogMessage Info "Finding channel #name ..",
                  LogMessage Info "Building channel cache before finding channel #name ..",
                  LogMessage Info "Finished building channel cache. Finding channel #name .."
                ],
                Right (Just Channel {_id = "id", _name = "name", _topic = "topic"})
              )
          )

    it "finds channel with a matching name on the second page" $ do
      Channels.find "page2_channel"
        & runGetPaginatedConst
          [ [trimming|
              {
                "channels": [{
                  "id": "id1",
                  "name": "page1_channel",
                  "topic": {
                    "value": "topic1"
                  }
                }]
              }
            |],
            [trimming|
              {
                "channels": [{
                  "id": "id2",
                  "name": "page2_channel",
                  "topic": {
                    "value": "topic2"
                  }
                }]
              }
            |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Right (Just Channel {_id = "id2", _name = "page2_channel", _topic = "topic2"}))

    context "for a program with multiple find invocations" $ do
      it "only calls conversations.list once" $ do
        let program =
              do
                first <- Channels.find "first"
                second <- Channels.find "second"
                return (first, second)

        countRef <- newIORef 0
        program
          & runWithExpectations \case
            GetPaginated _ _ -> modifyIORef countRef (+ 1)
            _ -> expectationFailure "Expected a GetPaginated query"
        count <- readIORef countRef
        count `shouldBe` 1

      it "logs the building and usage of the cache" $ do
        let program =
              do
                first <- Channels.find "first"
                second <- Channels.find "second"
                return (first, second)

        program
          & runGetPaginatedConst []
          & ( `shouldBe`
                ( [ LogMessage Info "Finding channel #first ..",
                    LogMessage Info "Building channel cache before finding channel #first ..",
                    LogMessage Info "Finished building channel cache. Finding channel #first ..",
                    LogMessage Info "Finding channel #second ..",
                    LogMessage Info "Using channel cache to find channel #second .."
                  ],
                  Right (Nothing, Nothing)
                )
            )

  describe "create" $ do
    it "posts to conversations.create" $ do
      Channels.create "name"
        & runWithExpectations \case
          Post _ method -> method `shouldBe` "conversations.create"
          _ -> expectationFailure "Expected a Post query"

    it "passes channel name paramater" $ do
      Channels.create "channel_name"
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("name", "channel_name")]
          _ -> expectationFailure "Expected a Post query"

    it "fails on a response of an empty object" $ do
      Channels.create "whatever"
        & runPostConst "{}"
        & snd -- Ignore logs
        & (`shouldBe` Left "\"conversations.create\" response didn't include a \"channel\" key")

    it "fails if channel object does not have an 'id' key" $ do
      Channels.create "whatever"
        & runPostConst "{\"channel\": {}}"
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"id\" not found")

    it "returns the Channel object" $ do
      Channels.create "name"
        & runPostConst
          [trimming|
            {
              "channel": {
                "id": "id",
                "name": "name",
                "topic": {
                  "value": "topic"
                }
              }
            }
          |]
        & ( `shouldBe`
              ( [ LogMessage Info "Creating channel #name ..",
                  LogMessage Info "Finished creating #name. Updating channel cache .."
                ],
                Right (Channel {_id = "id", _name = "name", _topic = "topic"})
              )
          )

  describe "setTopic" $ do
    it "posts to conversations.setTopic" $ do
      Channels.setTopic "channel_id" "topic_message"
        & runWithExpectations \case
          Post _ method -> method `shouldBe` "conversations.setTopic"
          _ -> expectationFailure "Expected a Post query"

    it "passes channel ID paramater" $ do
      Channels.setTopic "channel_id" "topic_message"
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("channel", "channel_id")]
          _ -> expectationFailure "Expected a Post query"

    it "passes topic message paramater" $ do
      Channels.setTopic "channel_id" "topic_message"
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("topic", "topic_message")]
          _ -> expectationFailure "Expected a Post query"

    it "ignores response body" $ do
      Channels.setTopic "channel_id" "topic_message"
        & runPostConst "{\"whatever\": {}}"
        & ( `shouldBe`
              ( [ LogMessage Info "Setting channel (ID: channel_id) topic to: topic_message ..",
                  LogMessage Info "Finished setting channel (ID: channel_id) topic. Updating channel cache .."
                ],
                Right ()
              )
          )

runWithExpectations = runSlackWithExpectations @Effects runChannels

runPostConst page = runSlackWith runChannels (nullSlackMatch {postResponse = Just page})

runGetPaginatedConst pages = runSlackWith runChannels (nullSlackMatch {getPaginatedResponse = Just pages})
