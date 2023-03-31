{-# LANGUAGE QuasiQuotes #-}

module ConfigSpec (spec) where

import Config (Conf (..), Rotation (..), apply, resolve, runConfig)
import Config qualified (parse)
import Control.Arrow (second)
import Data.Function ((&))
import Data.Functor ((<&>))
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
import Polysemy.State (State, get, modify, runState)
import Slack.Channel (Channel (..), Channels (..))
import Slack.Channel qualified as C (Channels (Create, Find))
import Slack.Group (Group (..), Groups (..))
import Slack.Group qualified as G (Groups (Create, Find))
import Slack.User (Users (..))
import Slack.User qualified as U (Users (Find, ListAll))
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
    it "checks and filters out already applied Weekly rotations" $ do
      let confList =
            [ Conf
                { rotation = Weekly [["weekly@example.com"]],
                  effects = []
                },
              Conf
                { rotation = Const ["const@example.com"],
                  effects = []
                },
              Conf
                { rotation = OpsgenieScheduleID "schedule-id",
                  effects = []
                }
            ]
      time <- iso8601ParseM "2021-10-10T00:00:00Z"

      resolve confList
        & runOpsgenie (\_ -> return ["opsgenie@example.com"])
        & runTimeConst time
        & runDeduplicationStoreConst True
        & interpretLogNull
        & run
        <&> second effects
        & ( `shouldMatchList`
              [ ( Set.fromList ["const@example.com"],
                  []
                ),
                ( Set.fromList ["opsgenie@example.com"],
                  []
                )
              ]
          )

    it "deduplicates subsequent runs of same conf for Weekly rotations" $ do
      let confList =
            [ Conf
                { rotation = Weekly [["weekly@example.com"]],
                  effects = []
                },
              Conf
                { rotation = Const ["const@example.com"],
                  effects = []
                },
              Conf
                { rotation = OpsgenieScheduleID "schedule-id",
                  effects = []
                }
            ]
      time <- iso8601ParseM "2021-10-10T00:00:00Z"

      let program = do
            firstResolve <- resolve confList
            apply firstResolve
            secondResolve <- resolve confList
            return (fmap (second effects) firstResolve, fmap (second effects) secondResolve)

      program
        & runOpsgenie (\_ -> return ["opsgenie@example.com"])
        & runTimeConst time
        & runDeduplicationStoreInMemory
        & runChannelsNull
        & runUsersNull
        & runGroupsNull
        & runError
        & interpretLogNull
        & run
        & ( `shouldBe`
              Right
                ( [ -- First time
                    ( Set.fromList ["weekly@example.com"],
                      []
                    ),
                    ( Set.fromList ["const@example.com"],
                      []
                    ),
                    ( Set.fromList ["opsgenie@example.com"],
                      []
                    )
                  ],
                  [ -- Second time
                    ( Set.fromList ["const@example.com"],
                      []
                    ),
                    ( Set.fromList ["opsgenie@example.com"],
                      []
                    )
                  ]
                )
          )

runChannelsNull :: InterpreterFor Channels r
runChannelsNull = interpret \case
  C.Create name -> return Channel {_id = "", _name = name, _topic = ""}
  SetTopic _id _topic -> return ()
  C.Find _name -> return Nothing

runUsersNull :: InterpreterFor Users r
runUsersNull = interpret \case
  U.Find _emailToFind -> return Nothing
  U.ListAll -> return []

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

runTimeConst :: UTCTime -> InterpreterFor Time r
runTimeConst time = interpret \case
  GetCurrent -> return time

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
          <&> maybe False (sameAs ctx)
      StoreAppliedContext ctx -> modify $ Map.insert (effectsHash ctx) ctx
    sameAs a b = (effectsHash a, inputHash a) == (effectsHash b, inputHash b)
