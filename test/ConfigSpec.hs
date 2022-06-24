{-# LANGUAGE QuasiQuotes #-}

module ConfigSpec (spec) where

import Config
  ( Conf,
    Effect (..),
    currentResolvedRotationEffects,
    runConfig,
  )
import qualified Config (parse)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601ParseM)
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
          let Effect = ./src/Effect.dhall
          let Rotation = ./src/Rotation.dhall
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

  it "returns a group with SetSlackGroup effect" $ do
    confList <-
      parseConfList
        [trimming|
          let Effect = ./src/Effect.dhall
          let Rotation = ./src/Rotation.dhall
           in [ { rotation = Rotation.Const ["user-id"]
                , effects = [ Effect.SetSlackGroup
                                { handle = "group-handle"
                                , name = "group-name"
                                }
                            ]
                }
              ]
        |]
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList
    resolvedRotationEffectsList
      `shouldMatchList` [ ( Set.fromList ["user-id"],
                            [ SetSlackGroup
                                { handle = "group-handle",
                                  name = "group-name"
                                }
                            ]
                          )
                        ]

  it "resolves weekly rotation" $ do
    confList <-
      parseConfList
        [trimming|
          let Effect = ./src/Effect.dhall
          let Rotation = ./src/Rotation.dhall
           in [ { rotation = Rotation.Weekly [["user-id-one", "user-id-two"]]
                , effects = [ Effect.SetSlackGroup
                                { handle = "group-handle"
                                , name = "group-name"
                                }
                            ]
                }
              ]
        |]
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList
    resolvedRotationEffectsList
      `shouldMatchList` [ ( Set.fromList ["user-id-one"],
                            [ SetSlackGroup
                                { handle = "group-handle",
                                  name = "group-name"
                                }
                            ]
                          )
                        ]

parseConfList :: Text -> IO [Conf]
parseConfList = runM . runConfig . Config.parse
