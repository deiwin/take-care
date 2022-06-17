{-# LANGUAGE QuasiQuotes #-}

module ConfigSpec (spec) where

import Config
  ( Conf,
    Group (..),
    currentGroups,
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
  it "returns no groups without SetSlackGroup effect" $ do
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

    let groups = currentGroups time confList
    groups `shouldMatchList` []

  it "returns a group with SetSlackGroup effect" $ do
    confList <-
      parseConfList
        [trimming|
        let Effect = ./src/Effect.dhall
        let Rotation = ./src/Rotation.dhall
         in [ { rotation = Rotation.Const ["user-id"]
              , effects = [ Effect.SetSlackGroup "group-name" ]
              }
            ]
      |]
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    let groups = currentGroups time confList
    groups
      `shouldMatchList` [ Group
                            { handle = "group-name",
                              description = "mock-description",
                              memberIDs = Set.fromList ["user-id"]
                            }
                        ]

parseConfList :: Text -> IO [Conf]
parseConfList = runM . runConfig . Config.parse
