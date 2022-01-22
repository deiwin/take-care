{-# LANGUAGE QuasiQuotes #-}

module Slack.ChannelSpec (spec) where

import Control.Lens ((&), (^.))
import NeatInterpolation (trimming)
import Network.Wreq (params)
import Slack.Channel
  ( Channel (..),
    runChannels,
  )
import qualified Slack.Channel as Channels (create, find, setTopic)
import Slack.TestUtils
  ( SlackResponse (..),
    nullSlackMatch,
    runSlackWith,
    runSlackWithExpectations,
  )
import Slack.Util (Slack (..))
import Test.Hspec
  ( Spec,
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
      Channels.find "whatever"
        & runGetPaginatedConst ["{}"]
        & (`shouldBe` Left "\"conversations.list\" response didn't include a \"channels\" field")

    it "retuns Nothing if the list of channels is empty" $ do
      Channels.find "whatever"
        & runGetPaginatedConst ["{\"channels\": []}"]
        & (`shouldBe` Right Nothing)

    it "fails if channel object does not have an 'id' key" $ do
      Channels.find "whatever"
        & runGetPaginatedConst ["{\"channels\": [{}]}"]
        & (`shouldBe` Left "key \"id\" not found")

    it "fails if channel object does not have a 'name' key" $ do
      Channels.find "whatever"
        & runGetPaginatedConst ["{\"channels\": [{\"id\": \"something\"}]}"]
        & (`shouldBe` Left "key \"name\" not found")

    it "fails if channel object does not have a 'topic' key" $ do
      Channels.find "whatever"
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
        & (`shouldBe` Left "key \"topic\" not found")

    it "fails if 'topic' key is not an object" $ do
      Channels.find "whatever"
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
        & (`shouldBe` Left "parsing HashMap ~Text failed, expected Object, but encountered String")

    it "fails if topic object does not have a 'value' key" $ do
      Channels.find "whatever"
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
        & (`shouldBe` Left "key \"value\" not found")

    it "returns Nothing if the channel ID doesn't match the query" $ do
      Channels.find "whatever"
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
        & (`shouldBe` Right Nothing)

    it "returns the Channel object if name is a match" $ do
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
        & (`shouldBe` Right (Just (Channel {_id = "id", _name = "name", _topic = "topic"})))

    it "returns the Channel object for a match on the second page" $ do
      Channels.find "name"
        & runGetPaginatedConst
          [ "{\"channels\": []}",
            [trimming|
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
        & (`shouldBe` Right (Just (Channel {_id = "id", _name = "name", _topic = "topic"})))

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
        & (`shouldBe` Left "\"conversations.create\" response didn't include a \"channel\" key")

    it "fails if channel object does not have an 'id' key" $ do
      Channels.create "whatever"
        & runPostConst "{\"channel\": {}}"
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
        & (`shouldBe` Right (Channel {_id = "id", _name = "name", _topic = "topic"}))

  describe "setTopic" $ do
    it "posts to conversations.setTopic" $ do
      Channels.setTopic "channel_name" "topic_message"
        & runWithExpectations \case
          Post _ method -> method `shouldBe` "conversations.setTopic"
          _ -> expectationFailure "Expected a Post query"

    it "passes channel name paramater" $ do
      Channels.setTopic "channel_name" "topic_message"
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("channel", "channel_name")]
          _ -> expectationFailure "Expected a Post query"

    it "passes topic message paramater" $ do
      Channels.setTopic "channel_name" "topic_message"
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("topic", "topic_message")]
          _ -> expectationFailure "Expected a Post query"

    it "ignores response body" $ do
      Channels.setTopic "channel_name" "topic_message"
        & runPostConst "{\"whatever\": {}}"
        & (`shouldBe` Right ())

runWithExpectations = runSlackWithExpectations runChannels

runPostConst page = runSlackWith runChannels (nullSlackMatch {postResponse = Just page})

runGetPaginatedConst pages = runSlackWith runChannels (nullSlackMatch {getPaginatedResponse = Just pages})
