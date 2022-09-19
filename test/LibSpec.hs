{-# LANGUAGE QuasiQuotes #-}

module LibSpec (spec) where

import Config (Conf (..), Config (..), Effect (..), Rotation (..))
import Control.Arrow (first, second)
import Control.Category ((>>>))
import Control.Lens ((^.))
import Data.Function ((&))
import Data.List (find)
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
import Polysemy.Log (Log, LogMessage (..), Severity (..), interpretLogNull, interpretLogOutput)
import Polysemy.Output (Output, runOutputList)
import Polysemy.State (State, get, modify, put, runState)
import Slack.Channel (Channel (..), Channels (..))
import qualified Slack.Channel as C (Channels (Create, Find))
import Slack.Group (Group (..), Groups (..))
import qualified Slack.Group as G (Groups (Create, Find))
import Slack.User (User (..), Users (..))
import qualified Slack.User as U (Users (Find, ListAll), id)
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
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Fetching all users ..",
                  LogMessage Info "Finished fetching all users"
                ],
                ""
              )
          )

    it "pretty prints the user IDs and display names" $ do
      listUsers
        & runListUsersConst
          [ User {_id = "id1", _displayName = "@name1"},
            User {_id = "id2", _displayName = "@name2"}
          ]
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Fetching all users ..",
                  LogMessage Info "Finished fetching all users"
                ],
                "id1: @name1\n\
                \id2: @name2\n"
              )
          )

  describe "dryRunEnsure" $ do
    it "returns an error if a configured user ID does not exist" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      dryRunEnsure "ignored"
        & runConfigConst
          [ Conf
              { rotation = Weekly [["user_id"]],
                effects =
                  [ SlackSetGroup
                      { handle = "team-caretaker",
                        name = "Team team caretaker(s)",
                        channels = []
                      },
                    SlackSetChannelTopic
                      { name = "tm-team",
                        topic = const "topic"
                      }
                  ]
              }
          ]
        & runTimeConst time
        & runListUsersConst []
        & runError
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Parsing configuration ..",
                  LogMessage Info "Resolving current time ..",
                  LogMessage Info "Resolving rotation effects ..",
                  LogMessage Info "Showing resolved rotation effects .."
                ],
                Left "Could not find user with ID: user_id"
              )
          )

    it "returns the pretty-printed output of changes to be made" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      dryRunEnsure "ignored"
        & runConfigConst
          [ Conf
              { rotation = Weekly [["alice", "bob"]],
                effects =
                  [ SlackSetGroup
                      { handle = "team-caretaker",
                        name = "Team team caretaker(s)",
                        channels = []
                      },
                    SlackSetChannelTopic
                      { name = "tm-team",
                        topic = const "topic"
                      }
                  ]
              },
            Conf
              { rotation = Const ["alice", "bob", "caroline"],
                effects =
                  [ SlackSetGroup
                      { handle = "team-team",
                        name = "Team team",
                        channels = ["tm-team"]
                      }
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
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Parsing configuration ..",
                  LogMessage Info "Resolving current time ..",
                  LogMessage Info "Resolving rotation effects ..",
                  LogMessage Info "Showing resolved rotation effects .."
                ],
                Right
                  [trimming|
                    For Alice:
                      SlackSetGroup: @team-caretaker {name = "Team team caretaker(s)", channels = []}
                      SlackSetChannelTopic #tm-team: topic

                    For Alice, Bob, Caroline:
                      SlackSetGroup: @team-team {name = "Team team", channels = ["tm-team"]}
                  |]
              )
          )

  describe "ensure" $ do
    it "creates necessary groups and channels" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      ensure "ignored"
        & runConfigConst
          [ Conf
              { rotation = Weekly [["alice", "bob"]],
                effects =
                  [ SlackSetGroup
                      { handle = "design-caretaker",
                        name = "Team design caretaker(s)",
                        channels = []
                      },
                    SlackSetChannelTopic
                      { name = "tm-design",
                        topic = ("Caretaker is: " <>) . intercalate ", "
                      }
                  ]
              },
            Conf
              { rotation = Const ["alice", "bob", "caroline"],
                effects =
                  [ SlackSetGroup
                      { handle = "design-team",
                        name = "Team design",
                        channels = ["tm-design"]
                      }
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
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Parsing configuration ..",
                  LogMessage Info "Resolving current time ..",
                  LogMessage Info "Applying all configurations ..",
                  LogMessage Info "Applying all effects for a rotation ..",
                  LogMessage Info "Applying to member IDs fromList [\"alice\"] the effect SlackSetGroup {handle = \"design-caretaker\", name = \"Team design caretaker(s)\", channels = []} ..",
                  LogMessage Info "Finding or creating the following channels: [] ..",
                  LogMessage Info "Finding or creating the group @design-caretaker ..",
                  LogMessage Info "Updating group members if changed from fromList [] to fromList [\"alice\"] ..",
                  LogMessage Info "Updating default channel IDs if changed from [] to [] ..",
                  LogMessage Info "Finished applying effect SlackSetGroup {handle = \"design-caretaker\", name = \"Team design caretaker(s)\", channels = []}",
                  LogMessage Info "Applying to member IDs fromList [\"alice\"] the effect SlackSetChannelTopic {name = \"tm-design\", topic = \"Caretaker is: input 1, input 2, ..\"} ..",
                  LogMessage Info "Finding or creating channel #tm-design ..",
                  LogMessage Info "Updating topic if changed from \"\" to \"Caretaker is: Alice\" ..",
                  LogMessage Info "Finished applying effect SlackSetChannelTopic {name = \"tm-design\", topic = \"Caretaker is: input 1, input 2, ..\"}",
                  LogMessage Info "Applying all effects for a rotation ..",
                  LogMessage Info "Applying to member IDs fromList [\"alice\",\"bob\",\"caroline\"] the effect SlackSetGroup {handle = \"design-team\", name = \"Team design\", channels = [\"tm-design\"]} ..",
                  LogMessage Info "Finding or creating the following channels: [\"tm-design\"] ..",
                  LogMessage Info "Finding or creating the group @design-team ..",
                  LogMessage Info "Updating group members if changed from fromList [] to fromList [\"alice\",\"bob\",\"caroline\"] ..",
                  LogMessage Info "Updating default channel IDs if changed from [\"id_tm-design\"] to [\"id_tm-design\"] ..",
                  LogMessage Info "Finished applying effect SlackSetGroup {handle = \"design-team\", name = \"Team design\", channels = [\"tm-design\"]}",
                  LogMessage Info "Completed applying all configurations"
                ],
                Right
                  ( Map.fromList
                      [ ( "id_design-caretaker",
                          ( Group
                              { _id = "id_design-caretaker",
                                _handle = "design-caretaker",
                                _name = "Team design caretaker(s)",
                                _channelIDs = []
                              },
                            ["alice"]
                          )
                        ),
                        ( "id_design-team",
                          ( Group
                              { _id = "id_design-team",
                                _handle = "design-team",
                                _name = "Team design",
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
                      ()
                    )
                  )
              )
          )

    it "does not try to create the same channel twice" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      ensure "ignored"
        & runConfigConst
          [ Conf
              { rotation = Const ["user-name"],
                effects =
                  [ SlackSetGroup
                      { handle = "group-handle",
                        name = "group-name",
                        channels = ["channel-name"]
                      },
                    SlackSetGroup
                      { handle = "second-group-handle",
                        name = "second-group-name",
                        channels = ["channel-name"]
                      }
                  ]
              }
          ]
        & runTimeConst time
        & runListUsersConst
          [User {_id = "user-name", _displayName = "user-display-name"}]
        & runChannels Map.empty
        & runGroups Map.empty
        & runError
        & interpretLogNull
        & run
        & ( `shouldBe`
              Right
                ( Map.fromList
                    [ ( "id_group-handle",
                        ( Group
                            { _id = "id_group-handle",
                              _handle = "group-handle",
                              _name = "group-name",
                              _channelIDs = ["id_channel-name"]
                            },
                          ["user-name"]
                        )
                      ),
                      ( "id_second-group-handle",
                        ( Group
                            { _id = "id_second-group-handle",
                              _handle = "second-group-handle",
                              _name = "second-group-name",
                              _channelIDs = ["id_channel-name"]
                            },
                          ["user-name"]
                        )
                      )
                    ],
                  ( Map.fromList
                      [ ( "id_channel-name",
                          Channel
                            { _id = "id_channel-name",
                              _name = "channel-name",
                              _topic = ""
                            }
                        )
                      ],
                    ()
                  )
                )
          )

type ChannelStore = (Map Text Channel)

runChannels ::
  Member (Error Text) r =>
  ChannelStore ->
  Sem (Channels ': State ChannelStore ': r) a ->
  Sem r (ChannelStore, a)
runChannels initialState =
  stateInterpreter
    >>> runState initialState
  where
    stateInterpreter ::
      ( Member (State ChannelStore) r,
        Member (Error Text) r
      ) =>
      InterpreterFor Channels r
    stateInterpreter = interpret \case
      C.Create name ->
        let id = "id_" <> name
            channel = Channel {_id = id, _name = name, _topic = ""}
         in do
              map <- get
              case Map.lookup id map of
                Just _ -> throw ("Trying to create a channel that already exists with ID: " <> id)
                Nothing -> do
                  put $ Map.insert id channel map
                  return channel
      SetTopic id topic ->
        let updateTopic channel = channel {_topic = topic}
         in modify $ Map.adjust updateTopic id
      C.Find name -> Map.lookup ("id_" <> name) <$> get

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
      G.Create handle name defaultChannelIDs ->
        let groupID = "id_" <> handle
            group = Group {_id = groupID, _handle = handle, _name = name, _channelIDs = defaultChannelIDs}
            members = []
         in do
              map <- get
              case Map.lookup groupID map of
                Just _ -> throw ("Trying to create a group that already exists with ID: " <> groupID)
                Nothing -> do
                  put $ Map.insert groupID (group, members) map
                  return group
      G.Find handle -> fmap fst . Map.lookup ("id_" <> handle) <$> get

runConfigConst :: [Conf] -> InterpreterFor Config r
runConfigConst teams = interpret \case
  Parse _ -> return teams

runTimeConst :: UTCTime -> InterpreterFor Time r
runTimeConst time = interpret \case
  GetCurrent -> return time

runListUsersConst :: [User] -> InterpreterFor Users r
runListUsersConst users = interpret \case
  U.Find idToFind -> return $ find ((== idToFind) . (^. U.id)) users
  U.ListAll -> return users

runLogToList :: Sem (Log ': Output LogMessage ': r) a -> Sem r ([LogMessage], a)
runLogToList = runOutputList . interpretLogOutput
