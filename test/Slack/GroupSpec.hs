{-# LANGUAGE QuasiQuotes #-}

module Slack.GroupSpec (spec) where

import Control.Category ((>>>))
import Control.Lens ((&), (^.))
import Control.Monad (void)
import Data.Aeson (Value (Null), decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import NeatInterpolation (trimming)
import Network.Wreq (params)
import Polysemy (Embed, InterpreterFor, Member, Sem, embed, interpret, run, runM)
import Polysemy.Error (Error, note, runError)
import Slack.Group
  ( Group (..),
    Groups,
    runGroups,
  )
import qualified Slack.Group as Groups (find)
import Slack.Util (Slack (..))
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
  )

spec :: Spec
spec = do
  describe "find" $ do
    it "queries usergroups.list" $ do
      Groups.find "handle"
        & runWithExpectations \case
          Get _ method -> method `shouldBe` "usergroups.list"
          _ -> expectationFailure "Expected a Get query"

    it "includes disabled groups" $ do
      Groups.find "handle"
        & runWithExpectations \case
          Get opts _ -> opts ^. params `shouldContain` [("include_disabled", "true")]
          _ -> expectationFailure "Expected a Get query"

    it "fails on a response of an empty object" $ do
      Groups.find "whatever"
        & runGetConst "{}"
        & (`shouldBe` Left "\"usergroups.list\" response didn't include a \"usergroups\" field")

    it "retuns Nothing if the list of groups is empty" $ do
      Groups.find "whatever"
        & runGetConst "{\"usergroups\": []}"
        & (`shouldBe` Right Nothing)

    it "fails if group object does not have an 'id' key" $ do
      Groups.find "whatever"
        & runGetConst "{\"usergroups\": [{}]}"
        & (`shouldBe` Left "key \"id\" not found")

    it "fails if group object does not have a 'handle' key" $ do
      Groups.find "whatever"
        & runGetConst "{\"usergroups\": [{\"id\": \"something\"}]}"
        & (`shouldBe` Left "key \"handle\" not found")

    it "fails if group object does not have a 'prefs' key" $ do
      Groups.find "whatever"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle"
            }]
          }
          |]
        & (`shouldBe` Left "key \"prefs\" not found")

    it "fails if 'prefs' key is not an object" $ do
      Groups.find "whatever"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle",
              "prefs": "prefs"
            }]
          }
          |]
        & (`shouldBe` Left "parsing HashMap ~Text failed, expected Object, but encountered String")

    it "fails if prefs object does not have a 'channels' key" $ do
      Groups.find "whatever"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle",
              "prefs": {}
            }]
          }
          |]
        & (`shouldBe` Left "key \"channels\" not found")

    it "returns Nothing if the group handle doesn't match the query" $ do
      Groups.find "whatever"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle",
              "prefs": {
                "channels": ["channel_id1", "channel_id2"]
              }
            }]
          }
          |]
        & (`shouldBe` Right Nothing)

    it "returns the group object if handle is a match" $ do
      Groups.find "group_handle"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle",
              "prefs": {
                "channels": ["channel_id1", "channel_id2"]
              }
            }]
          }
          |]
        & ( `shouldBe`
              Right
                ( Just
                    ( Group
                        { _id = "group_id",
                          _handle = "group_handle",
                          _channelIDs = ["channel_id1", "channel_id2"]
                        }
                    )
                )
          )

runWithExpectations ::
  (forall rInitial x. (Slack (Sem rInitial) x -> IO ())) ->
  Sem '[Groups, Slack, Error Text, Embed IO] a ->
  Expectation
runWithExpectations expectations =
  runGroups
    >>> runSlackWithExpectations
    >>> runError
    >>> runM
    >>> void
  where
    runSlackWithExpectations :: Member (Embed IO) r => Sem (Slack ': r) a -> Sem r a
    runSlackWithExpectations = interpret (embed . expecationWithNull)
    -- Required to match the type expectation of interpret
    expecationWithNull :: Slack (Sem rInitial) x -> IO x
    expecationWithNull slackQuery =
      case slackQuery of
        Get _ _ -> expectations slackQuery $> Null
        GetPaginated _ _ -> expectations slackQuery $> []
        Post _ _ -> expectations slackQuery $> Null

runGetConst :: Text -> Sem '[Groups, Slack, Error Text] a -> Either Text a
runGetConst page = runGroupsWith (nullSlackMatch {getResponse = Just page})

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

runGroupsWith ::
  SlackResponse ->
  Sem
    '[ Groups,
       Slack,
       Error Text
     ]
    a ->
  Either Text a
runGroupsWith slackResponse =
  runGroups
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
