{-# LANGUAGE QuasiQuotes #-}

module Slack.UserSpec (spec) where

import Data.Function ((&))
import Data.IORef (modifyIORef, newIORef, readIORef)
import NeatInterpolation (trimming)
import Polysemy.Log (LogMessage (..), Severity (..))
import Slack.TestUtils
  ( SlackResponse (..),
    nullSlackMatch,
    runSlackWith,
    runSlackWithExpectations,
  )
import Slack.User
  ( Effects,
    User (..),
    runUsers,
  )
import qualified Slack.User as Users (find, listAll)
import Slack.Util (Slack (..))
import Test.Hspec
  ( Spec,
    context,
    describe,
    expectationFailure,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "listAll" $ do
    it "queries users.list" $ do
      Users.listAll
        & runWithExpectations \case
          GetPaginated _ method -> method `shouldBe` "users.list"
          _ -> expectationFailure "Expected a GetPaginated query"

    it "fails on a response of an empty object" $ do
      Users.listAll
        & runGetPaginatedConst ["{}"]
        & snd -- Ignore logs
        & (`shouldBe` Left "\"users.list\" response didn't include a \"members\" field")

    it "retuns an empty list if the list of users is empty" $ do
      Users.listAll
        & runGetPaginatedConst ["{\"members\": []}"]
        & snd -- Ignore logs
        & (`shouldBe` Right [])

    it "fails if user object does not have an 'id' key" $ do
      Users.listAll
        & runGetPaginatedConst ["{\"members\": [{}]}"]
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"id\" not found")

    it "fails if user object does not have a 'name' key" $ do
      Users.listAll
        & runGetPaginatedConst ["{\"members\": [{\"id\": \"something\"}]}"]
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"name\" not found")

    it "fails if user object does not have a 'profile' key" $ do
      Users.listAll
        & runGetPaginatedConst
          [ [trimming|
            {
              "members": [{
                "id": "id",
                "name": "name"
              }]
            }
            |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Left "key \"profile\" not found")

    it "fails if 'profile' key is not an object" $ do
      Users.listAll
        & runGetPaginatedConst
          [ [trimming|
            {
              "members": [{
                "id": "id",
                "name": "name",
                "profile": "profile"
              }]
            }
          |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Left "parsing KeyMap failed, expected Object, but encountered String")

    it "uses 'name' field if profile object does not have a 'display_name' key" $ do
      Users.listAll
        & runGetPaginatedConst
          [ [trimming|
            {
              "members": [{
                "id": "id",
                "name": "name",
                "profile": {}
              }]
            }
          |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Right [User {_id = "id", _displayName = "@name"}])

    it "uses 'name' field if 'display_name' is an empty string" $ do
      Users.listAll
        & runGetPaginatedConst
          [ [trimming|
            {
              "members": [{
                "id": "id",
                "name": "name",
                "profile": {
                  "display_name": ""
                }
              }]
            }
          |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Right [User {_id = "id", _displayName = "@name"}])

    it "returns the User objects" $ do
      Users.listAll
        & runGetPaginatedConst
          [ [trimming|
              {
                "members": [{
                  "id": "id",
                  "name": "name",
                  "profile": {
                    "display_name": "display_name"
                  }
                }, {
                  "id": "id2",
                  "name": "name2",
                  "profile": {
                    "display_name": "display_name2"
                  }
                }]
              }
            |]
          ]
        & ( `shouldBe`
              ( [ LogMessage Info "Listing all users ..",
                  LogMessage Info "Building user cache .."
                ],
                Right
                  [ User {_id = "id", _displayName = "@display_name"},
                    User {_id = "id2", _displayName = "@display_name2"}
                  ]
              )
          )

    it "returns a User object from the second page" $ do
      Users.listAll
        & runGetPaginatedConst
          [ "{\"members\": []}",
            [trimming|
              {
                "members": [{
                  "id": "id",
                  "name": "name",
                  "profile": {
                    "display_name": "display_name"
                  }
                }]
              }
            |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Right [User {_id = "id", _displayName = "@display_name"}])

  describe "find" $ do
    it "returns Nothing when user not found" $ do
      Users.find "missing_id"
        & runGetPaginatedConst
          [ [trimming|
              {
                "members": [{
                  "id": "id",
                  "name": "name",
                  "profile": {
                    "display_name": "display_name"
                  }
                }, {
                  "id": "id2",
                  "name": "name2",
                  "profile": {
                    "display_name": "display_name2"
                  }
                }]
              }
            |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Right Nothing)

    it "returns the right user" $ do
      Users.find "id2"
        & runGetPaginatedConst
          [ [trimming|
              {
                "members": [{
                  "id": "id",
                  "name": "name",
                  "profile": {
                    "display_name": "display_name"
                  }
                }, {
                  "id": "id2",
                  "name": "name2",
                  "profile": {
                    "display_name": "display_name2"
                  }
                }]
              }
            |]
          ]
        & ( `shouldBe`
              ( [ LogMessage Info "Finding user with ID id2 ..",
                  LogMessage Info "Building user cache .."
                ],
                Right (Just (User {_id = "id2", _displayName = "@display_name2"}))
              )
          )

    it "returns the right user from the second page" $ do
      Users.find "id2"
        & runGetPaginatedConst
          [ [trimming|
              {
                "members": [{
                  "id": "id",
                  "name": "name",
                  "profile": {
                    "display_name": "display_name"
                  }
                }]
              }
            |],
            [trimming|
              {
                "members": [{
                  "id": "id2",
                  "name": "name2",
                  "profile": {
                    "display_name": "display_name2"
                  }
                }]
              }
            |]
          ]
        & snd -- Ignore logs
        & (`shouldBe` Right (Just (User {_id = "id2", _displayName = "@display_name2"})))

  context "for a program with multiple find and listAll calls" $ do
    it "only calls users.list once for multiple find and listAll calls" $ do
      let program =
            do
              first <- Users.find "first_ID"
              all <- Users.listAll
              second <- Users.find "second_ID"
              return (first, all, second)

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
              first <- Users.find "first_ID"
              all <- Users.listAll
              second <- Users.find "second_ID"
              return (first, all, second)

      program
        & runGetPaginatedConst []
        & ( `shouldBe`
              ( [ LogMessage Info "Finding user with ID first_ID ..",
                  LogMessage Info "Building user cache ..",
                  LogMessage Info "Listing all users ..",
                  LogMessage Info "Using cached user map ..",
                  LogMessage Info "Finding user with ID second_ID ..",
                  LogMessage Info "Using cached user map .."
                ],
                Right (Nothing, [], Nothing)
              )
          )

runWithExpectations = runSlackWithExpectations @Effects runUsers

runGetPaginatedConst pages = runSlackWith runUsers (nullSlackMatch {getPaginatedResponse = Just pages})
