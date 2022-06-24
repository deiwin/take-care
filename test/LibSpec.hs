{-# LANGUAGE QuasiQuotes #-}

module LibSpec (spec) where

import Config (Conf (..), Config (..), Effect (..), Rotation (..))
import Control.Arrow (first, second)
import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, intercalate)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import IO (Time (..))
import Lib (dryRunEnsure, ensure, listUsers)
import NeatInterpolation (trimming)
import Polysemy (InterpreterFor, Member, Sem, interpret, run)
import qualified Polysemy as P (Members)
import Polysemy.Error (Error, note, runError, throw)
import Polysemy.State (State, get, modify, put, runState)
import Slack.Channel (Channel (..), Channels (..))
import qualified Slack.Channel as C (Channels (Create, ListAll))
import Slack.Group (Group (..), Groups (..))
import qualified Slack.Group as G (Groups (Create))
import Slack.User (User (..), Users (..))
import qualified Slack.User as U (Users (ListAll))
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
          [ Conf
              { rotation = Weekly [["user_id"]],
                effects =
                  [ SetSlackGroup "team-caretaker",
                    SetSlackChannelTopic
                      { name = "tm-team",
                        topic = const "topic"
                      }
                  ]
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
          [ Conf
              { rotation = Weekly [["alice", "bob"]],
                effects =
                  [ SetSlackGroup "team-caretaker",
                    SetSlackChannelTopic
                      { name = "tm-team",
                        topic = const "topic"
                      }
                  ]
              },
            Conf
              { rotation = Const ["alice", "bob", "caroline"],
                effects =
                  [ SetSlackGroup "team-team",
                    InviteToSlackChannel "tm-team"
                  ]
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
                  For Alice:
                    SetSlackGroup: @team-caretaker
                    SetSlackChannelTopic #tm-team: topic

                  For Alice, Bob, Caroline:
                    SetSlackGroup: @team-team
                    InviteToSlackChannel: #tm-team
                |]
          )

  describe "ensure" $ do
    it "creates necessary groups and channels" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      ensure "ignored"
        & runConfigConst
          [ Conf
              { rotation = Weekly [["alice", "bob"]],
                effects =
                  [ SetSlackGroup "design-caretaker",
                    SetSlackChannelTopic
                      { name = "tm-design",
                        topic = ("Caretaker is: " <>) . intercalate ", "
                      }
                  ]
              },
            Conf
              { rotation = Const ["alice", "bob", "caroline"],
                effects =
                  [ SetSlackGroup "design-team",
                    InviteToSlackChannel "tm-design"
                  ]
              }
          ]
        & runTimeConst time
        & runListUsersConst
          [ User {_id = "alice", _displayName = "Alice"},
            User {_id = "bob", _displayName = "Bob"},
            User {_id = "caroline", _displayName = "Caroline"}
          ]
        & runChannels Map.empty
        & runGroups Map.empty
        & runError
        & run
        & ( `shouldBe`
              Right
                ( Map.fromList
                    [ ( "id_design-caretaker",
                        ( Group
                            { _id = "id_design-caretaker",
                              _handle = "design-caretaker",
                              _channelIDs = ["id_tm-design"]
                            },
                          ["alice"]
                        )
                      ),
                      ( "id_design-team",
                        ( Group
                            { _id = "id_design-team",
                              _handle = "design-team",
                              _channelIDs = ["id_tm-design"]
                            },
                          ["alice", "bob", "caroline"]
                        )
                      )
                    ],
                  ( Map.fromList
                      [ ( "id_tm-design",
                          Channel
                            { _id = "id_tm-design",
                              _name = "tm-design",
                              _topic = "Caretaker is: Alice"
                            }
                        )
                      ],
                    "Team design: success!\n"
                  )
                )
          )

type ChannelStore = (Map Text Channel)

runChannels :: ChannelStore -> Sem (Channels ': State ChannelStore ': r) a -> Sem r (ChannelStore, a)
runChannels initialState =
  stateInterpreter
    >>> runState initialState
  where
    stateInterpreter :: Member (State ChannelStore) r => InterpreterFor Channels r
    stateInterpreter = interpret \case
      C.Create name ->
        let id = "id_" <> name
            channel = Channel {_id = id, _name = name, _topic = ""}
         in do
              modify $ Map.insert id channel
              return channel
      C.ListAll -> Map.elems <$> get
      SetTopic id topic ->
        let updateTopic channel = channel {_topic = topic}
         in modify $ Map.adjust updateTopic id

type GroupStore = (Map Text (Group, [Text]))

runGroups ::
  Member (Error Text) r =>
  GroupStore ->
  Sem (Groups ': State GroupStore ': r) a ->
  Sem r (GroupStore, a)
runGroups initialState =
  stateInterpreter
    >>> runState initialState
  where
    stateInterpreter :: P.Members '[State GroupStore, Error Text] r => InterpreterFor Groups r
    stateInterpreter = interpret \case
      GetMembers groupID -> do
        store <- get
        Map.lookup groupID store
          & fmap snd
          & note ("Group " <> groupID <> " does not exist")
      SetMembers groupID userIDs -> do
        store <- get
        if Map.member groupID store
          then put $ Map.adjust (second (const userIDs)) groupID store
          else throw ("Group " <> groupID <> " does not exist")
      SetChannels groupID channelIDs ->
        let updateChannels group = group {_channelIDs = channelIDs}
         in do
              store <- get
              if Map.member groupID store
                then put $ Map.adjust (first updateChannels) groupID store
                else throw ("Group " <> groupID <> " does not exist")
      G.Create handle _name defaultChannelIDs ->
        let groupID = "id_" <> handle
            group = Group {_id = groupID, _handle = handle, _channelIDs = defaultChannelIDs}
            members = []
         in do
              modify $ Map.insert groupID (group, members)
              return group
      Find handle -> fmap fst . Map.lookup ("id_" <> handle) <$> get

runConfigConst :: [Conf] -> InterpreterFor Config r
runConfigConst teams = interpret \case
  Parse _ -> return teams

runTimeConst :: UTCTime -> InterpreterFor Time r
runTimeConst time = interpret \case
  GetCurrent -> return time

runListUsersConst :: [User] -> InterpreterFor Users r
runListUsersConst users = interpret \case
  U.ListAll -> return users
