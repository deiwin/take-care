{-# LANGUAGE QuasiQuotes #-}

module Slack.ChannelSpec (spec) where

import Control.Category ((>>>))
import Data.Aeson (Value, decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import NeatInterpolation (trimming)
import Polysemy (InterpreterFor, Member, Sem, interpret, run)
import Polysemy.Error (Error, note, runError)
import Slack.Channel
  ( Channel (..),
    Channels,
    runChannels,
  )
import qualified Slack.Channel as Channels (find)
import Slack.Util (Slack (..))
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "find" $ do
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

runGetPaginatedConst ::
  [Text] ->
  Sem
    '[ Channels,
       Slack,
       Error Text
     ]
    a ->
  Either Text a
runGetPaginatedConst pages = runChannelsWith (constGetPaginated pages)

constGetPaginated :: [Text] -> SlackResponse
constGetPaginated pages = nullSlackMatch {getPaginatedResponse = Just pages}

data SlackResponse = SlackResponse
  { getResponse :: Maybe Text,
    getPaginatedResponse :: Maybe [Text],
    postResponse :: Maybe Text
  }

nullSlackMatch :: SlackResponse
nullSlackMatch =
  SlackResponse
    { getResponse = Nothing,
      getPaginatedResponse = Nothing,
      postResponse = Nothing
    }

runChannelsWith ::
  SlackResponse ->
  Sem
    '[ Channels,
       Slack,
       Error Text
     ]
    a ->
  Either Text a
runChannelsWith slackResponse =
  runChannels
    >>> runSlack slackResponse
    >>> runError
    >>> run

json :: Member (Error Text) r => Text -> Sem r Value
json =
  encodeUtf8
    >>> fromStrict
    >>> decode
    >>> note "failed to parse test JSON"

runSlack :: Member (Error Text) r => SlackResponse -> InterpreterFor Slack r
runSlack slackResponse = interpret \case
  Get _ _ ->
    getResponse slackResponse
      & note "No response provided for Get"
      & (>>= json)
  GetPaginated _ _ ->
    getPaginatedResponse slackResponse
      & note "No response provided for GetPaginated"
      & (>>= traverse json)
  Post _ _ ->
    postResponse slackResponse
      & note "No response provided for Post"
      & (>>= json)
