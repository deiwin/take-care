{-# LANGUAGE QuasiQuotes #-}

module ConfigSpec (spec) where

import Config (Conf (..), Rotation (..), apply, resolve, runConfig)
import Config qualified (parse)
import Control.Arrow (second)
import Control.Lens ((^.))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import DeduplicationStore (DeduplicationContext (..), DeduplicationStore (..))
import Effect (Effect (..))
import Effect.Slack (SlackEffect (..))
import IO (Time (..))
import NeatInterpolation (trimming)
import Opsgenie (Opsgenie (..))
import Polysemy (InterpreterFor, InterpretersFor, Member, Sem, interpret, run, runM)
import Polysemy.Error (runError)
import Polysemy.Log (interpretLogNull)
import Polysemy.State (State, get, modify, put, runState)
import Slack.Channel (Channel (..), Channels (..))
import Slack.Channel qualified as C (Channels (Create, Find))
import Slack.Group (Group (..), Groups (..))
import Slack.Group qualified as G (Groups (Create, Find))
import Slack.User (User (..), Users (..))
import Slack.User qualified as U (Users (Find, ListAll), email)
import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList)
import Prelude hiding (lines, readFile, unlines)

spec :: Spec
spec = do
  it "returns an empty list for an empty list of effects" $ do
    confList <-
      parseConfList
        [trimming|
          let Effect = ./types/core/Effect.dhall
          let Rotation = ./types/core/Rotation.dhall
           in [ { rotation = Rotation.Const ["whatever"]
                , effects = [] : List Effect
                }
              ]
        |]
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    resolve confList
      & runTimeConst time
      & runOpsgenieFail
      & runDeduplicationStoreConst False
      & interpretLogNull
      & run
      <&> second effects
      & ( `shouldMatchList`
            [ ( Set.fromList ["whatever"],
                []
              )
            ]
        )

  it "returns a group with Slack.SetGroup effect" $ do
    confList <-
      parseConfList
        [trimming|
          let SlackEffect = ./types/core/Effect/Slack.dhall
          let Effect = ./types/core/Effect.dhall
          let Rotation = ./types/core/Rotation.dhall
           in [ { rotation = Rotation.Const ["user-id"]
                , effects = [ Effect.Slack (SlackEffect.SetGroup
                                { handle = "group-handle"
                                , name = "group-name"
                                , channels = ["channel-name"]
                                })
                            ]
                }
              ]
        |]
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    resolve confList
      & runTimeConst time
      & runOpsgenieFail
      & runDeduplicationStoreConst False
      & interpretLogNull
      & run
      <&> second effects
      & ( `shouldMatchList`
            [ ( Set.fromList ["user-id"],
                [ Slack
                    SetGroup
                      { handle = "group-handle",
                        name = "group-name",
                        channels = ["channel-name"]
                      }
                ]
              )
            ]
        )

  it "resolves weekly rotation" $ do
    confList <-
      parseConfList
        [trimming|
          let Effect = ./types/core/Effect.dhall
          let Rotation = ./types/core/Rotation.dhall
           in [ { rotation = Rotation.Weekly [["user-id-one", "user-id-two"]]
                , effects = [] : List Effect
                }
              ]
        |]
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    resolve confList
      & runTimeConst time
      & runOpsgenieFail
      & runDeduplicationStoreConst False
      & interpretLogNull
      & run
      <&> second effects
      & ( `shouldMatchList`
            [ ( Set.fromList ["user-id-one"],
                []
              )
            ]
        )

  it "resolves OpsgenieScheduleID rotation" $ do
    confList <-
      parseConfList
        [trimming|
          let Effect = ./types/core/Effect.dhall
          let Rotation = ./types/core/Rotation.dhall
           in [ { rotation = Rotation.OpsgenieScheduleID "schedule-id"
                , effects = [] : List Effect
                }
              ]
        |]
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    resolve confList
      & runOpsgenie
        ( \id ->
            if id == "schedule-id"
              then return ["user@example.com"]
              else error "Unexpected schedule ID"
        )
      & runTimeConst time
      & runDeduplicationStoreConst False
      & interpretLogNull
      & run
      <&> second effects
      & ( `shouldMatchList`
            [ ( Set.fromList ["user@example.com"],
                []
              )
            ]
        )

  describe "deduplication" $ do
    it "checks and filters out all already applied rotations" $ do
      let confList =
            [ Conf
                { rotation = Weekly [["weekly@example.com"]],
                  effects =
                    [ Slack
                        SetChannelTopic
                          { name = "weekly-channel",
                            topic = const "weekly"
                          }
                    ]
                },
              Conf
                { rotation = Const ["const@example.com"],
                  effects =
                    [ Slack
                        SetChannelTopic
                          { name = "const-channel",
                            topic = const "const"
                          }
                    ]
                },
              Conf
                { rotation = OpsgenieScheduleID "schedule-id",
                  effects =
                    [ Slack
                        SetChannelTopic
                          { name = "opsgenie-channel",
                            topic = const "opsgenie"
                          }
                    ]
                }
            ]
      time <- iso8601ParseM "2021-10-10T00:00:00Z"

      resolve confList
        & runOpsgenie (\_ -> return ["opsgenie@example.com"])
        & runTimeConst time
        & runDeduplicationStoreConst True
        & interpretLogNull
        & run
        <&> fst
        & (`shouldBe` [])

    it "deduplicates subsequent runs of same conf for Weekly rotations" $ do
      let confList =
            [ Conf
                { rotation = Weekly [["weekly@example.com"]],
                  effects =
                    [ Slack
                        SetChannelTopic
                          { name = "weekly-channel",
                            topic = const "weekly"
                          }
                    ]
                },
              Conf
                { rotation = Const ["const@example.com"],
                  effects =
                    [ Slack
                        SetChannelTopic
                          { name = "const-channel",
                            topic = const "const"
                          }
                    ]
                },
              Conf
                { rotation = OpsgenieScheduleID "schedule-id",
                  effects =
                    [ Slack
                        SetChannelTopic
                          { name = "opsgenie-channel",
                            topic = const "opsgenie"
                          }
                    ]
                }
            ]
      time <- iso8601ParseM "2021-10-10T00:00:00Z"

      let program = do
            firstResolve <- resolve confList
            apply firstResolve
            secondResolve <- resolve confList
            return (fmap fst firstResolve, fmap fst secondResolve)

      program
        & runOpsgenie (\_ -> return ["opsgenie@example.com"])
        & runTimeConst time
        & runDeduplicationStoreInMemory
        & runChannelsNull
        & runUsersConst
          [ User {_id = "1", _displayName = "Weekly", _email = "weekly@example.com"},
            User {_id = "2", _displayName = "Const", _email = "const@example.com"},
            User {_id = "3", _displayName = "Opsgenie", _email = "opsgenie@example.com"}
          ]
        & runGroupsNull
        & runError
        & interpretLogNull
        & run
        & ( `shouldBe`
              Right
                ( [ -- First time
                    Set.fromList ["weekly@example.com"],
                    Set.fromList ["const@example.com"],
                    Set.fromList ["opsgenie@example.com"]
                  ],
                  [] -- Second time
                )
          )

    it "applies Weekly conf again after the week changes" $ do
      let confList =
            [ Conf
                { rotation = Weekly [["one@example.com", "two@example.com"]],
                  effects = []
                }
            ]

      initialTime <- iso8601ParseM "2021-10-10T00:00:00Z"
      secondTime <- iso8601ParseM "2021-10-17T00:00:00Z"
      let program = do
            firstResolve <- resolve confList
            apply firstResolve

            put secondTime

            secondResolve <- resolve confList
            return (fmap fst firstResolve, fmap fst secondResolve)

      program
        & runOpsgenieFail
        & runTimeState
        & fmap snd . runState initialTime
        & runDeduplicationStoreInMemory
        & runChannelsNull
        & runUsersConst
          [ User {_id = "1", _displayName = "One", _email = "one@example.com"},
            User {_id = "2", _displayName = "Two", _email = "two@example.com"}
          ]
        & runGroupsNull
        & runError
        & interpretLogNull
        & run
        & ( `shouldBe`
              Right
                ( [Set.fromList ["one@example.com"]], -- First time
                  [Set.fromList ["two@example.com"]] -- Second time
                )
          )

    it "applies Const conf again after the input changes" $ do
      let initialConfList = [Conf {rotation = Const ["one@example.com"], effects = []}]
      let secondConfList = [Conf {rotation = Const ["two@example.com"], effects = []}]

      time <- iso8601ParseM "2021-10-10T00:00:00Z"

      let program = do
            firstResolve <- resolve initialConfList
            apply firstResolve

            secondResolve <- resolve secondConfList
            return (fmap fst firstResolve, fmap fst secondResolve)

      program
        & runOpsgenieFail
        & runTimeConst time
        & runDeduplicationStoreInMemory
        & runChannelsNull
        & runUsersConst
          [ User {_id = "1", _displayName = "One", _email = "one@example.com"},
            User {_id = "2", _displayName = "Two", _email = "two@example.com"}
          ]
        & runGroupsNull
        & runError
        & interpretLogNull
        & run
        & ( `shouldBe`
              Right
                ( [Set.fromList ["one@example.com"]], -- First time
                  [Set.fromList ["two@example.com"]] -- Second time
                )
          )

    it "applies Opsgenie conf again after the information returned from Opsgenie changes" $ do
      let confList = [Conf {rotation = OpsgenieScheduleID "schedule-id", effects = []}]

      time <- iso8601ParseM "2021-10-10T00:00:00Z"

      let initialResponse = ["one@example.com"]
      let secondResponse = ["two@example.com"]

      let program = do
            firstResolve <- resolve confList
            apply firstResolve

            put secondResponse

            secondResolve <- resolve confList
            return (fmap fst firstResolve, fmap fst secondResolve)

      program
        & runOpsgenieState
        & fmap snd . runState initialResponse
        & runTimeConst time
        & runDeduplicationStoreInMemory
        & runChannelsNull
        & runUsersConst
          [ User {_id = "1", _displayName = "One", _email = "one@example.com"},
            User {_id = "2", _displayName = "Two", _email = "two@example.com"}
          ]
        & runGroupsNull
        & runError
        & interpretLogNull
        & run
        & ( `shouldBe`
              Right
                ( [Set.fromList ["one@example.com"]], -- First time
                  [Set.fromList ["two@example.com"]] -- Second time
                )
          )

runChannelsNull :: InterpreterFor Channels r
runChannelsNull = interpret \case
  C.Create name -> return Channel {_id = "", _name = name, _topic = ""}
  SetTopic _id _topic -> return ()
  C.Find _name -> return Nothing

runUsersConst :: [User] -> InterpreterFor Users r
runUsersConst users = interpret \case
  U.Find emailToFind -> return $ find ((== emailToFind) . (^. U.email)) users
  U.ListAll -> return users

runGroupsNull :: InterpreterFor Groups r
runGroupsNull = interpret \case
  GetMembers _groupID -> return []
  SetMembers _groupID _userIDs -> return ()
  SetChannels _groupID _channelIDs -> return ()
  G.Create handle name defaultChannelIDs ->
    return $ Group {_id = "", _handle = handle, _name = name, _channelIDs = defaultChannelIDs}
  G.Find _handle -> return Nothing

runOpsgenieFail :: InterpreterFor Opsgenie r
runOpsgenieFail = runOpsgenie (\_ -> error "Expected Opsgenie not to be called")

runOpsgenie :: (Text -> Sem r [Text]) -> InterpreterFor Opsgenie r
runOpsgenie f = interpret \case
  WhoIsOnCall scheduleID -> f scheduleID

runOpsgenieState :: (Member (State [Text]) r) => InterpreterFor Opsgenie r
runOpsgenieState = interpret \case
  WhoIsOnCall _scheduleID -> get

runTimeConst :: UTCTime -> InterpreterFor Time r
runTimeConst time = interpret \case
  GetCurrent -> return time

runTimeState :: (Member (State UTCTime) r) => InterpreterFor Time r
runTimeState = interpret \case
  GetCurrent -> get

parseConfList :: Text -> IO [Conf]
parseConfList = runM . runConfig . Config.parse

runDeduplicationStoreConst :: Bool -> InterpreterFor DeduplicationStore r
runDeduplicationStoreConst bool = interpret \case
  IsAlreadyApplied _ -> return bool
  StoreAppliedContext _ -> return ()

type DedupeStore = (Map Text DeduplicationContext)

runDeduplicationStoreInMemory :: InterpretersFor '[DeduplicationStore, State DedupeStore] r
runDeduplicationStoreInMemory = fmap snd . runState Map.empty . interpreter
  where
    interpreter :: (Member (State DedupeStore) r) => InterpreterFor DeduplicationStore r
    interpreter = interpret \case
      IsAlreadyApplied ctx ->
        get
          <&> Map.lookup (effectsHash ctx)
          <&> (== Just ctx)
      StoreAppliedContext ctx -> modify $ Map.insert (effectsHash ctx) ctx
