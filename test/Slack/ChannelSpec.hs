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
        & runPaginatedWith ["{}"]
        & (`shouldBe` Left "\"conversations.list\" response didn't include a \"channels\" field")

    it "retuns Nothing if the list of channels is empty" $ do
      Channels.find "whatever"
        & runPaginatedWith ["{\"channels\": []}"]
        & (`shouldBe` Right Nothing)

    it "fails if channel object does not have an 'id' key" $ do
      Channels.find "whatever"
        & runPaginatedWith ["{\"channels\": [{}]}"]
        & (`shouldBe` Left "key \"id\" not found")

    it "fails if channel object does not have a 'name' key" $ do
      Channels.find "whatever"
        & runPaginatedWith ["{\"channels\": [{\"id\": \"something\"}]}"]
        & (`shouldBe` Left "key \"name\" not found")

    it "fails if channel object does not have a 'topic' key" $ do
      Channels.find "whatever"
        & runPaginatedWith
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
        & runPaginatedWith
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
        & runPaginatedWith
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
        & runPaginatedWith
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
        & runPaginatedWith
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
        & runPaginatedWith
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

runPaginatedWith ::
  [Text] ->
  Sem
    '[ Channels,
       Slack,
       Error Text
     ]
    a ->
  Either Text a
runPaginatedWith outputs =
  runChannels
    >>> runSlack outputs
    >>> runError
    >>> run

json :: Member (Error Text) r => Text -> Sem r Value
json =
  encodeUtf8
    >>> fromStrict
    >>> decode
    >>> note "failed to parse test JSON"

runSlack :: Member (Error Text) r => [Text] -> InterpreterFor Slack r
runSlack outputs = interpret \case
  Get _ _ -> undefined
  GetPaginated _opts _method -> traverse json outputs
  Post _ _ -> undefined
