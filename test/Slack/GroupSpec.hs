{-# LANGUAGE QuasiQuotes #-}

module Slack.GroupSpec (spec) where

import Control.Lens ((&), (^.))
import NeatInterpolation (trimming)
import Network.Wreq (params)
import Polysemy.Log (LogMessage (..), Severity (..))
import Slack.Group
  ( Group (..),
    Groups,
    runGroups,
  )
import qualified Slack.Group as Groups
  ( create,
    find,
    getMembers,
    setChannels,
    setMembers,
  )
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
        & snd -- Ignore logs
        & (`shouldBe` Left "\"usergroups.list\" response didn't include a \"usergroups\" field")

    it "retuns Nothing if the list of groups is empty" $ do
      Groups.find "whatever"
        & runGetConst "{\"usergroups\": []}"
        & snd -- Ignore logs
        & (`shouldBe` Right Nothing)

    it "fails if group object does not have an 'id' key" $ do
      Groups.find "whatever"
        & runGetConst "{\"usergroups\": [{}]}"
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"id\" not found")

    it "fails if group object does not have a 'handle' key" $ do
      Groups.find "whatever"
        & runGetConst "{\"usergroups\": [{\"id\": \"something\"}]}"
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"handle\" not found")

    it "fails if group object does not have a name key" $ do
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
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"name\" not found")

    it "fails if group object does not have a 'prefs' key" $ do
      Groups.find "whatever"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle",
              "name": "group_name"
            }]
          }
          |]
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"prefs\" not found")

    it "fails if 'prefs' key is not an object" $ do
      Groups.find "whatever"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle",
              "name": "group_name",
              "prefs": "prefs"
            }]
          }
          |]
        & snd -- Ignore logs
        & (`shouldBe` Left "parsing KeyMap failed, expected Object, but encountered String")

    it "fails if prefs object does not have a 'channels' key" $ do
      Groups.find "whatever"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle",
              "name": "group_name",
              "prefs": {}
            }]
          }
          |]
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"channels\" not found")

    it "returns Nothing if the group handle doesn't match the query" $ do
      Groups.find "whatever"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle",
              "name": "group_name",
              "prefs": {
                "channels": ["channel_id1", "channel_id2"]
              }
            }]
          }
          |]
        & snd -- Ignore logs
        & (`shouldBe` Right Nothing)

    it "returns the group object if handle is a match" $ do
      Groups.find "group_handle"
        & runGetConst
          [trimming|
          {
            "usergroups": [{
              "id": "group_id",
              "handle": "group_handle",
              "name": "group_name",
              "prefs": {
                "channels": ["channel_id1", "channel_id2"]
              }
            }]
          }
          |]
        & ( `shouldBe`
              ( [LogMessage Info "Finding group by handle @group_handle .."],
                Right
                  ( Just
                      ( Group
                          { _id = "group_id",
                            _handle = "group_handle",
                            _name = "group_name",
                            _channelIDs = ["channel_id1", "channel_id2"]
                          }
                      )
                  )
              )
          )

  describe "getMembers" $ do
    it "queries usergroups.users.list" $ do
      Groups.getMembers "id"
        & runWithExpectations \case
          Get _ method -> method `shouldBe` "usergroups.users.list"
          _ -> expectationFailure "Expected a Get query"

    it "specifies the group ID" $ do
      let groupId = "group_id"
      Groups.getMembers groupId
        & runWithExpectations \case
          Get opts _ -> opts ^. params `shouldContain` [("usergroup", groupId)]
          _ -> expectationFailure "Expected a Get query"

    it "includes disabled groups" $ do
      Groups.getMembers "id"
        & runWithExpectations \case
          Get opts _ -> opts ^. params `shouldContain` [("include_disabled", "true")]
          _ -> expectationFailure "Expected a Get query"

    it "returns the list of user IDs if such usergroup does exist" $ do
      Groups.getMembers "whatever"
        & runGetConst
          [trimming|
          {
            "ok": true,
            "users": ["user1", "user2"]
          }
          |]
        & ( `shouldBe`
              ( [LogMessage Info "Getting members of group with ID whatever .."],
                Right ["user1", "user2"]
              )
          )

  describe "setMembers" $ do
    it "queries usergroups.users.update" $ do
      Groups.setMembers "group_id" ["alice", "bob"]
        & runWithExpectations \case
          Post _ method -> method `shouldBe` "usergroups.users.update"
          _ -> expectationFailure "Expected a Post query"

    it "passes group ID paramater" $ do
      Groups.setMembers "group_id" ["alice", "bob"]
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("usergroup", "group_id")]
          _ -> expectationFailure "Expected a Post query"

    it "passes user IDs as a comma-separated list" $ do
      Groups.setMembers "group_id" ["alice", "bob"]
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("users", "alice,bob")]
          _ -> expectationFailure "Expected a Post query"

    it "ignores response body" $ do
      Groups.setMembers "group_id" ["alice", "bob"]
        & runPostConst "{\"whatever\": {}}"
        & ( `shouldBe`
              ( [LogMessage Info "Setting members of group with ID group_id to user IDs [\"alice\",\"bob\"] .."],
                Right ()
              )
          )

  describe "setChannels" $ do
    it "queries usergroups.update" $ do
      Groups.setChannels "group_id" ["channel1", "channel2"]
        & runWithExpectations \case
          Post _ method -> method `shouldBe` "usergroups.update"
          _ -> expectationFailure "Expected a Post query"

    it "passes group ID paramater" $ do
      Groups.setChannels "group_id" ["channel1", "channel2"]
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("usergroup", "group_id")]
          _ -> expectationFailure "Expected a Post query"

    it "passes channel IDs as a comma-separated list" $ do
      Groups.setChannels "group_id" ["channel1", "channel2"]
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("channels", "channel1,channel2")]
          _ -> expectationFailure "Expected a Post query"

    it "ignores response body" $ do
      Groups.setChannels "group_id" ["channel1", "channel2"]
        & runPostConst "{\"whatever\": {}}"
        & ( `shouldBe`
              ( [LogMessage Info "Setting channels of group with ID group_id to channel IDs [\"channel1\",\"channel2\"] .."],
                Right ()
              )
          )

  describe "create" $ do
    it "queries usergroups.create" $ do
      Groups.create "group_handle" "Group Name" ["channel1", "channel2"]
        & runWithExpectations \case
          Post _ method -> method `shouldBe` "usergroups.create"
          _ -> expectationFailure "Expected a Post query"

    it "passes group handle paramater" $ do
      Groups.create "group_handle" "Group Name" ["channel1", "channel2"]
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("handle", "group_handle")]
          _ -> expectationFailure "Expected a Post query"

    it "passes group name paramater" $ do
      Groups.create "group_handle" "Group Name" ["channel1", "channel2"]
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("name", "Group Name")]
          _ -> expectationFailure "Expected a Post query"

    it "passes channel IDs as a comma-separated list" $ do
      Groups.create "group_handle" "Group Name" ["channel1", "channel2"]
        & runWithExpectations \case
          Post params _ -> params `shouldContain` [("channels", "channel1,channel2")]
          _ -> expectationFailure "Expected a Post query"

    it "fails on a response of an empty object" $ do
      Groups.create "group_handle" "Group Name" ["channel1", "channel2"]
        & runPostConst "{}"
        & snd -- Ignore logs
        & (`shouldBe` Left "\"usergroups.create\" response didn't include a \"usergroup\" key")

    it "returns the created group object" $ do
      Groups.create "group_handle" "Group Name" ["channel1", "channel2"]
        & runPostConst
          [trimming|
          {
            "usergroup": {
              "id": "group_id",
              "handle": "group_handle",
              "name": "group_name",
              "prefs": {
                "channels": ["channel1", "channel2"]
              }
            }
          }
          |]
        & ( `shouldBe`
              ( [LogMessage Info "Creating a group with handle @group_handle .."],
                Right
                  ( Group
                      { _id = "group_id",
                        _handle = "group_handle",
                        _name = "group_name",
                        _channelIDs = ["channel1", "channel2"]
                      }
                  )
              )
          )

runWithExpectations = runSlackWithExpectations @'[Groups] runGroups

runPostConst page = runSlackWith runGroups (nullSlackMatch {postResponse = Just page})

runGetConst page = runSlackWith runGroups (nullSlackMatch {getResponse = Just page})
