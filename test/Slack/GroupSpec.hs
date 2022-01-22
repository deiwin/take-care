{-# LANGUAGE QuasiQuotes #-}

module Slack.GroupSpec (spec) where

import Control.Lens ((&), (^.))
import NeatInterpolation (trimming)
import Network.Wreq (params)
import Slack.Group
  ( Group (..),
    runGroups,
  )
import qualified Slack.Group as Groups (find)
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

runWithExpectations = runSlackWithExpectations runGroups

runGetConst page = runSlackWith runGroups (nullSlackMatch {getResponse = Just page})
