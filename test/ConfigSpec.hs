{-# LANGUAGE QuasiQuotes #-}

module ConfigSpec (spec) where

import Config (Conf, resolve, runConfig)
import Config qualified (parse)
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Effect (Effect (..))
import Effect.Slack (SlackEffect (..))
import IO (Time (..))
import NeatInterpolation (trimming)
import Opsgenie (Opsgenie (..))
import Polysemy (InterpreterFor, Sem, interpret, run, runM)
import Test.Hspec (Spec, it, shouldMatchList)
import Prelude hiding (lines, readFile, unlines)
import Polysemy.Log (interpretLogNull)

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
      & interpretLogNull
      & run
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
      & interpretLogNull
      & run
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
      & runOpsgenieFail
      & runTimeConst time
      & interpretLogNull
      & run
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
      & interpretLogNull
      & run
      & ( `shouldMatchList`
            [ ( Set.fromList ["user@example.com"],
                []
              )
            ]
        )

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
