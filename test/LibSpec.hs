{-# LANGUAGE QuasiQuotes #-}

module LibSpec (spec) where

import Config (Conf (..), Config (..), Rotation (..))
import Control.Arrow (first, second)
import Control.Category ((>>>))
import Control.Lens ((^.))
import Data.Function ((&))
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, intercalate)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import DeduplicationStore (DeduplicationStore (IsAlreadyApplied, StoreAppliedContext))
import Effect (Effect (..))
import Effect.Slack (SlackEffect (..))
import IO (Time (..))
import Lib (dryRunEnsure, ensure, listUsers)
import NeatInterpolation (trimming)
import Opsgenie (Opsgenie (..))
import Polysemy (InterpreterFor, Member, Sem, interpret, run)
import Polysemy qualified as P (Members)
import Polysemy.Error (Error, note, runError, throw)
import Polysemy.Log (Log, LogMessage (..), Severity (..), interpretLogNull, interpretLogOutput)
import Polysemy.Output (Output, runOutputList)
import Polysemy.State (State, get, modify, put, runState)
import Slack.Channel (Channel (..), Channels (..))
import Slack.Channel qualified as C (Channels (Create, Find))
import Slack.Group (Group (..), Groups (..))
import Slack.Group qualified as G (Groups (Create, Find))
import Slack.User (User (..), Users (..))
import Slack.User qualified as U (Users (Find, ListAll), email)
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
          [ User {_id = "id1", _displayName = "@name1", _email = "user1@example.com"},
            User {_id = "id2", _displayName = "@name2", _email = "user2@example.com"}
          ]
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Fetching all users ..",
                  LogMessage Info "Finished fetching all users"
                ],
                "user1@example.com: @name1\n\
                \user2@example.com: @name2\n"
              )
          )

  describe "dryRunEnsure" $ do
    it "returns an error if a configured user email does not exist" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      dryRunEnsure "ignored"
        & runConfigConst
          [ Conf
              { rotation = Weekly [["missing@example.com"]],
                effects =
                  [ Slack
                      SetGroup
                        { handle = "team-caretaker",
                          name = "Team team caretaker(s)",
                          channels = []
                        },
                    Slack
                      SetChannelTopic
                        { name = "tm-team",
                          topic = const "topic"
                        }
                  ]
              }
          ]
        & runTimeConst time
        & runListUsersConst []
        & runOpsgenieFail
        & runDeduplicationStoreNull
        & runError
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Parsing configuration ..",
                  LogMessage Info "Resolving rotation effects ..",
                  LogMessage Info "Resolving Weekly rotation for [[\"missing@example.com\"]] ..",
                  LogMessage Info "Resolved Weekly rotation. Current members are: [\"missing@example.com\"]",
                  LogMessage Info "Showing resolved rotation effects .."
                ],
                Left "Could not find user with email: missing@example.com"
              )
          )

    it "returns an error if user email returned from Opsgenie does not exist" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      dryRunEnsure "ignored"
        & runConfigConst
          [ Conf
              { rotation = OpsgenieScheduleID "schedule-id",
                effects =
                  [ Slack
                      SetChannelTopic
                        { name = "tm-team",
                          topic = const "topic"
                        }
                  ]
              }
          ]
        & runOpsgenieConst ["missing@example.com"]
        & runTimeConst time
        & runListUsersConst []
        & runDeduplicationStoreNull
        & runError
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Parsing configuration ..",
                  LogMessage Info "Resolving rotation effects ..",
                  LogMessage Info "Resolving OpsgenieScheduleID rotation for schedule ID schedule-id ..",
                  LogMessage Info "Resolved OpsgenieScheduleID rotation. Current members are: [\"missing@example.com\"]",
                  LogMessage Info "Showing resolved rotation effects .."
                ],
                Left "Could not find user with email: missing@example.com"
              )
          )

    it "returns the pretty-printed output of changes to be made" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      dryRunEnsure "ignored"
        & runConfigConst
          [ Conf
              { rotation = Weekly [["alice@example.com", "bob@example.com"]],
                effects =
                  [ Slack
                      SetGroup
                        { handle = "team-caretaker",
                          name = "Team team caretaker(s)",
                          channels = []
                        },
                    Slack
                      SetChannelTopic
                        { name = "tm-team",
                          topic = const "topic"
                        }
                  ]
              },
            Conf
              { rotation = Const ["alice@example.com", "bob@example.com", "caroline@example.com"],
                effects =
                  [ Slack
                      SetGroup
                        { handle = "team-team",
                          name = "Team team",
                          channels = ["tm-team"]
                        }
                  ]
              },
            Conf
              { rotation = OpsgenieScheduleID "schedule-id",
                effects =
                  [ Slack
                      SetGroup
                        { handle = "second-team",
                          name = "Second team",
                          channels = ["tm-second"]
                        },
                    Slack
                      SetChannelTopic
                        { name = "tm-second",
                          topic = const "topic"
                        }
                  ]
              }
          ]
        & runOpsgenieConst ["dave@example.com", "eve@example.com"]
        & runTimeConst time
        & runListUsersConst
          [ User {_id = "1111ALICE", _displayName = "Alice", _email = "alice@example.com"},
            User {_id = "222222BOB", _displayName = "Bob", _email = "bob@example.com"},
            User {_id = "3CAROLINE", _displayName = "Caroline", _email = "caroline@example.com"},
            User {_id = "44444DAVE", _displayName = "Dave", _email = "dave@example.com"},
            User {_id = "555555EVE", _displayName = "Eve", _email = "eve@example.com"}
          ]
        & runDeduplicationStoreNull
        & runError
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Parsing configuration ..",
                  LogMessage Info "Resolving rotation effects ..",
                  LogMessage Info "Resolving Weekly rotation for [[\"alice@example.com\",\"bob@example.com\"]] ..",
                  LogMessage Info "Resolved Weekly rotation. Current members are: [\"alice@example.com\"]",
                  LogMessage Info "Resolving Const rotation by returning all listed members: [\"alice@example.com\",\"bob@example.com\",\"caroline@example.com\"] ..",
                  LogMessage Info "Resolving OpsgenieScheduleID rotation for schedule ID schedule-id ..",
                  LogMessage Info "Resolved OpsgenieScheduleID rotation. Current members are: [\"dave@example.com\",\"eve@example.com\"]",
                  LogMessage Info "Showing resolved rotation effects .."
                ],
                Right
                  [trimming|
                    For alice@example.com:
                      Slack.SetGroup: @team-caretaker {name = "Team team caretaker(s)", channels = []}
                      Slack.SetChannelTopic #tm-team: topic

                    For alice@example.com, bob@example.com, caroline@example.com:
                      Slack.SetGroup: @team-team {name = "Team team", channels = ["tm-team"]}

                    For dave@example.com, eve@example.com:
                      Slack.SetGroup: @second-team {name = "Second team", channels = ["tm-second"]}
                      Slack.SetChannelTopic #tm-second: topic
                  |]
              )
          )

  describe "ensure" $ do
    it "creates necessary groups and channels" $ do
      time <- iso8601ParseM "2021-10-10T00:00:00Z"
      ensure "ignored"
        & runConfigConst
          [ Conf
              { rotation = Weekly [["alice@example.com", "bob@example.com"]],
                effects =
                  [ Slack
                      SetGroup
                        { handle = "design-caretaker",
                          name = "Team design caretaker(s)",
                          channels = []
                        },
                    Slack
                      SetChannelTopic
                        { name = "tm-design",
                          topic = ("Caretaker is: " <>) . intercalate ", "
                        }
                  ]
              },
            Conf
              { rotation = Const ["alice@example.com", "bob@example.com", "caroline@example.com"],
                effects =
                  [ Slack
                      SetGroup
                        { handle = "design-team",
                          name = "Team design",
                          channels = ["tm-design"]
                        }
                  ]
              },
            Conf
              { rotation = OpsgenieScheduleID "schedule-id",
                effects =
                  [ Slack
                      SetGroup
                        { handle = "second-team",
                          name = "Second team",
                          channels = ["tm-second"]
                        },
                    Slack
                      SetChannelTopic
                        { name = "tm-second",
                          topic = const "topic"
                        }
                  ]
              }
          ]
        & runOpsgenieConst ["dave@example.com", "eve@example.com"]
        & runTimeConst time
        & runListUsersConst
          [ User {_id = "1111ALICE", _displayName = "Alice", _email = "alice@example.com"},
            User {_id = "222222BOB", _displayName = "Bob", _email = "bob@example.com"},
            User {_id = "3CAROLINE", _displayName = "Caroline", _email = "caroline@example.com"},
            User {_id = "44444DAVE", _displayName = "Dave", _email = "dave@example.com"},
            User {_id = "555555EVE", _displayName = "Eve", _email = "eve@example.com"}
          ]
        & runChannels Map.empty
        & runGroups Map.empty
        & runDeduplicationStoreNull
        & runError
        & runLogToList
        & run
        & ( `shouldBe`
              ( [ LogMessage Info "Parsing configuration ..",
                  LogMessage Info "Resolving rotation effects ..",
                  LogMessage Info "Resolving Weekly rotation for [[\"alice@example.com\",\"bob@example.com\"]] ..",
                  LogMessage Info "Resolved Weekly rotation. Current members are: [\"alice@example.com\"]",
                  LogMessage Info "Resolving Const rotation by returning all listed members: [\"alice@example.com\",\"bob@example.com\",\"caroline@example.com\"] ..",
                  LogMessage Info "Resolving OpsgenieScheduleID rotation for schedule ID schedule-id ..",
                  LogMessage Info "Resolved OpsgenieScheduleID rotation. Current members are: [\"dave@example.com\",\"eve@example.com\"]",
                  LogMessage Info "Applying all configurations ..",
                  LogMessage Info "Applying all effects for a rotation ..",
                  LogMessage Info "Applying to members fromList [\"alice@example.com\"] the effect Slack (SetGroup {handle = \"design-caretaker\", name = \"Team design caretaker(s)\", channels = []}) ..",
                  LogMessage Info "Finding or creating the following channels: [] ..",
                  LogMessage Info "Finding or creating the group @design-caretaker ..",
                  LogMessage Info "Updating group member IDs from fromList [] to fromList [\"1111ALICE\"] ..",
                  LogMessage Info "Finished applying effect Slack (SetGroup {handle = \"design-caretaker\", name = \"Team design caretaker(s)\", channels = []})",
                  LogMessage Info "Applying to members fromList [\"alice@example.com\"] the effect Slack (SetChannelTopic {name = \"tm-design\", topic = \"Caretaker is: input 1, input 2, ..\"}) ..",
                  LogMessage Info "Finding or creating channel #tm-design ..",
                  LogMessage Info "Updating topic if changed from \"\" to \"Caretaker is: Alice\" ..",
                  LogMessage Info "Finished applying effect Slack (SetChannelTopic {name = \"tm-design\", topic = \"Caretaker is: input 1, input 2, ..\"})",
                  LogMessage Info "Applying all effects for a rotation ..",
                  LogMessage Info "Applying to members fromList [\"alice@example.com\",\"bob@example.com\",\"caroline@example.com\"] the effect Slack (SetGroup {handle = \"design-team\", name = \"Team design\", channels = [\"tm-design\"]}) ..",
                  LogMessage Info "Finding or creating the following channels: [\"tm-design\"] ..",
                  LogMessage Info "Finding or creating the group @design-team ..",
                  LogMessage Info "Updating group member IDs from fromList [] to fromList [\"1111ALICE\",\"222222BOB\",\"3CAROLINE\"] ..",
                  LogMessage Info "Finished applying effect Slack (SetGroup {handle = \"design-team\", name = \"Team design\", channels = [\"tm-design\"]})",
                  LogMessage Info "Applying all effects for a rotation ..",
                  LogMessage Info "Applying to members fromList [\"dave@example.com\",\"eve@example.com\"] the effect Slack (SetGroup {handle = \"second-team\", name = \"Second team\", channels = [\"tm-second\"]}) ..",
                  LogMessage Info "Finding or creating the following channels: [\"tm-second\"] ..",
                  LogMessage Info "Finding or creating the group @second-team ..",
                  LogMessage Info "Updating group member IDs from fromList [] to fromList [\"44444DAVE\",\"555555EVE\"] ..",
                  LogMessage Info "Finished applying effect Slack (SetGroup {handle = \"second-team\", name = \"Second team\", channels = [\"tm-second\"]})",
                  LogMessage Info "Applying to members fromList [\"dave@example.com\",\"eve@example.com\"] the effect Slack (SetChannelTopic {name = \"tm-second\", topic = \"topic\"}) ..",
                  LogMessage Info "Finding or creating channel #tm-second ..",
                  LogMessage Info "Updating topic if changed from \"\" to \"topic\" ..",
                  LogMessage Info "Finished applying effect Slack (SetChannelTopic {name = \"tm-second\", topic = \"topic\"})",
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
                            ["1111ALICE"]
                          )
                        ),
                        ( "id_design-team",
                          ( Group
                              { _id = "id_design-team",
                                _handle = "design-team",
                                _name = "Team design",
                                _channelIDs = ["id_tm-design"]
                              },
                            ["1111ALICE", "222222BOB", "3CAROLINE"]
                          )
                        ),
                        ( "id_second-team",
                          ( Group
                              { _id = "id_second-team",
                                _handle = "second-team",
                                _name = "Second team",
                                _channelIDs = ["id_tm-second"]
                              },
                            ["44444DAVE", "555555EVE"]
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
                          ),
                          ( "id_tm-second",
                            Channel
                              { _id = "id_tm-second",
                                _name = "tm-second",
                                _topic = "topic"
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
              { rotation = Const ["user@example.com"],
                effects =
                  [ Slack
                      SetGroup
                        { handle = "group-handle",
                          name = "group-name",
                          channels = ["channel-name"]
                        },
                    Slack
                      SetGroup
                        { handle = "second-group-handle",
                          name = "second-group-name",
                          channels = ["channel-name"]
                        }
                  ]
              }
          ]
        & runTimeConst time
        & runListUsersConst
          [User {_id = "user-id", _displayName = "user-display-name", _email = "user@example.com"}]
        & runChannels Map.empty
        & runGroups Map.empty
        & runOpsgenieFail
        & runDeduplicationStoreNull
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
                          ["user-id"]
                        )
                      ),
                      ( "id_second-group-handle",
                        ( Group
                            { _id = "id_second-group-handle",
                              _handle = "second-group-handle",
                              _name = "second-group-name",
                              _channelIDs = ["id_channel-name"]
                            },
                          ["user-id"]
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
  U.Find emailToFind -> return $ find ((== emailToFind) . (^. U.email)) users
  U.ListAll -> return users

runLogToList :: Sem (Log ': Output LogMessage ': r) a -> Sem r ([LogMessage], a)
runLogToList = runOutputList . interpretLogOutput

runOpsgenieConst :: [Text] -> InterpreterFor Opsgenie r
runOpsgenieConst userEmails = interpret \case
  WhoIsOnCall _scheduleID -> return userEmails

runOpsgenieFail :: InterpreterFor Opsgenie r
runOpsgenieFail = interpret \case
  WhoIsOnCall _scheduleID -> error "Expected Opsgenie not to be called"

runDeduplicationStoreNull :: InterpreterFor DeduplicationStore r
runDeduplicationStoreNull = interpret \case
  IsAlreadyApplied _ -> return False
  StoreAppliedContext _ -> return ()
