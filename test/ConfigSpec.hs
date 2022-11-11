{-# LANGUAGE QuasiQuotes #-}

module ConfigSpec (spec) where

import Config
  ( Conf,
    currentResolvedRotationEffects,
    runConfig,
  )
import qualified Config (parse)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Effect (Effect (..))
import Effect.Slack (SlackEffect (..))
import NeatInterpolation (trimming)
import Polysemy (runM)
import Test.Hspec
  ( Spec,
    it,
    shouldMatchList,
  )
import Prelude hiding (lines, readFile, unlines)

spec :: Spec
spec = do
  it "returns no ResolvedRotationEffects for an empty list of effects" $ do
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

    let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList
    resolvedRotationEffectsList
      `shouldMatchList` [ ( Set.fromList ["whatever"],
                            []
                          )
                        ]

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

    let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList
    resolvedRotationEffectsList
      `shouldMatchList` [ ( Set.fromList ["user-id"],
                            [ Slack
                                SetGroup
                                  { handle = "group-handle",
                                    name = "group-name",
                                    channels = ["channel-name"]
                                  }
                            ]
                          )
                        ]

  it "resolves weekly rotation" $ do
    confList <-
      parseConfList
        [trimming|
          let SlackEffect = ./types/core/Effect/Slack.dhall
          let Effect = ./types/core/Effect.dhall
          let Rotation = ./types/core/Rotation.dhall
           in [ { rotation = Rotation.Weekly [["user-id-one", "user-id-two"]]
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

    let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList
    resolvedRotationEffectsList
      `shouldMatchList` [ ( Set.fromList ["user-id-one"],
                            [ Slack
                                SetGroup
                                  { handle = "group-handle",
                                    name = "group-name",
                                    channels = ["channel-name"]
                                  }
                            ]
                          )
                        ]

parseConfList :: Text -> IO [Conf]
parseConfList = runM . runConfig . Config.parse
