{-# LANGUAGE QuasiQuotes #-}

module LibSpec (spec) where

import Config (Config (..), Members (..), Team (..))
import Data.Function ((&))
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import IO (Time (..))
import Lib (dryRunEnsure, listUsers)
import NeatInterpolation (trimming)
import Polysemy (InterpreterFor, interpret, run)
import Polysemy.Error (runError)
import Slack.User (User (..), Users (..))
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "listUsers" $ do
    it "returns an empty string for an empty list of users" $ do
      listUsers
        & runListUsersConst []
        & run
        & (`shouldBe` "")

    it "pretty prints the user IDs and display names" $ do
      listUsers
        & runListUsersConst
          [ User {_id = "id1", _displayName = "@name1"},
            User {_id = "id2", _displayName = "@name2"}
          ]
        & run
        & ( `shouldBe`
              "id1: @name1\n\
              \id2: @name2\n"
          )

  describe "dryRunEnsure" $ do
    it "returns an error if a configured user ID does not exist" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      dryRunEnsure "ignored"
        & runConfigConst
          [ Team
              { members =
                  Members
                    { caretakers = [["user_id"]],
                      others = []
                    },
                team = "team",
                topic = const "topic"
              }
          ]
        & runTimeConst time
        & runListUsersConst []
        & runError
        & run
        & (`shouldBe` Left "Could not find user with ID: user_id")

    it "returns the pretty-printed output of changes to be made" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      dryRunEnsure "ignored"
        & runConfigConst
          [ Team
              { members =
                  Members
                    { caretakers = [["alice", "bob"]],
                      others = ["caroline"]
                    },
                team = "team",
                topic = const "topic"
              }
          ]
        & runTimeConst time
        & runListUsersConst
          [ User {_id = "alice", _displayName = "Alice"},
            User {_id = "bob", _displayName = "Bob"},
            User {_id = "caroline", _displayName = "Caroline"}
          ]
        & runError
        & run
        & ( `shouldBe`
              Right
                [trimming|
                  Team team:
                    #tm-team topic: topic
                    @team-caretaker group:
                      Description: Team team caretaker(s)
                      Members: Alice
                    @team-team group:
                      Description: Team team
                      Members: Alice, Bob, Caroline
                |]
          )

runConfigConst :: [Team] -> InterpreterFor Config r
runConfigConst teams = interpret \case
  Parse _ -> return teams

runTimeConst :: UTCTime -> InterpreterFor Time r
runTimeConst time = interpret \case
  GetCurrent -> return time

runListUsersConst :: [User] -> InterpreterFor Users r
runListUsersConst users = interpret \case
  ListAll -> return users
