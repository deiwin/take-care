{-# LANGUAGE QuasiQuotes #-}

module Slack.ChannelSpec (spec) where

import Control.Category ((>>>))
import Data.Aeson (Value, decode)
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy (fromStrict)
import Data.Function ((&))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import NeatInterpolation (trimming)
import Network.Wreq (Options)
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
import Text.Printf (printf)

spec :: Spec
spec = do
  describe "find" $ do
    it "fails on a response of an empty object" $ do
      Channels.find "whatever"
        & runChannelsWith (constGetPaginated ["{}"])
        & (`shouldBe` Left "\"conversations.list\" response didn't include a \"channels\" field")

    it "retuns Nothing if the list of channels is empty" $ do
      Channels.find "whatever"
        & runChannelsWith (constGetPaginated ["{\"channels\": []}"])
        & (`shouldBe` Right Nothing)

    it "fails if channel object does not have an 'id' key" $ do
      Channels.find "whatever"
        & runChannelsWith (constGetPaginated ["{\"channels\": [{}]}"])
        & (`shouldBe` Left "key \"id\" not found")

    it "fails if channel object does not have a 'name' key" $ do
      Channels.find "whatever"
        & runChannelsWith (constGetPaginated ["{\"channels\": [{\"id\": \"something\"}]}"])
        & (`shouldBe` Left "key \"name\" not found")

    it "fails if channel object does not have a 'topic' key" $ do
      Channels.find "whatever"
        & runChannelsWith
          ( constGetPaginated
              [ [trimming|
                   {
                     "channels": [{
                       "id": "id",
                       "name": "name"
                     }]
                   }
                |]
              ]
          )
        & (`shouldBe` Left "key \"topic\" not found")

    it "fails if 'topic' key is not an object" $ do
      Channels.find "whatever"
        & runChannelsWith
          ( constGetPaginated
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
          )
        & (`shouldBe` Left "parsing HashMap ~Text failed, expected Object, but encountered String")

    it "fails if topic object does not have a 'value' key" $ do
      Channels.find "whatever"
        & runChannelsWith
          ( constGetPaginated
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
          )
        & (`shouldBe` Left "key \"value\" not found")

    it "returns Nothing if the channel ID doesn't match the query" $ do
      Channels.find "whatever"
        & runChannelsWith
          ( constGetPaginated
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
          )
        & (`shouldBe` Right Nothing)

    it "returns the Channel object if name is a match" $ do
      Channels.find "name"
        & runChannelsWith
          ( constGetPaginated
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
          )
        & (`shouldBe` Right (Just (Channel {_id = "id", _name = "name", _topic = "topic"})))

    it "returns the Channel object for a match on the second page" $ do
      Channels.find "name"
        & runChannelsWith
          ( constGetPaginated
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
          )
        & (`shouldBe` Right (Just (Channel {_id = "id", _name = "name", _topic = "topic"})))

constGetPaginated :: [Text] -> SlackMatch
constGetPaginated pages = nullSlackMatch {getPaginatedMatch = [const2 (Just pages)]}

const2 :: a -> b -> c -> a
const2 a _ _ = a

type Match a b = a -> String -> Maybe b

data SlackMatch = SlackMatch
  { getMatch :: [Match Options Text],
    getPaginatedMatch :: [Match Options [Text]],
    postMatch :: [Match [Pair] Text]
  }
  deriving ()

nullSlackMatch :: SlackMatch
nullSlackMatch =
  SlackMatch
    { getMatch = [],
      getPaginatedMatch = [],
      postMatch = []
    }

runChannelsWith ::
  SlackMatch ->
  Sem
    '[ Channels,
       Slack,
       Error Text
     ]
    a ->
  Either Text a
runChannelsWith slackMatch =
  runChannels
    >>> runSlack slackMatch
    >>> runError
    >>> run

json :: Member (Error Text) r => Text -> Sem r Value
json =
  encodeUtf8
    >>> fromStrict
    >>> decode
    >>> note "failed to parse test JSON"

runSlack :: Member (Error Text) r => SlackMatch -> InterpreterFor Slack r
runSlack slackMatch = interpret \case
  Get opts method ->
    getMatch slackMatch
      & mapMaybe (\m -> m opts method)
      & listToMaybe
      & note (pack (printf "No Get match found for opts: %s, method: %s" (show opts) method))
      & (>>= json)
  GetPaginated opts method ->
    getPaginatedMatch slackMatch
      & mapMaybe (\m -> m opts method)
      & listToMaybe
      & note (pack (printf "No GetPaginated match found for opts: %s, method: %s" (show opts) method))
      & (>>= traverse json)
  Post opts method ->
    postMatch slackMatch
      & mapMaybe (\m -> m opts method)
      & listToMaybe
      & note (pack (printf "No Post match found for opts: %s, method: %s" (show opts) method))
      & (>>= json)
