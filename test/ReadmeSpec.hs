module ReadmeSpec (spec) where

import Config
  ( Group (..),
    Conf,
    currentDesiredTeamState,
    currentGroups,
    runConfig,
    showDesiredTeamStateList,
  )
import qualified Config (parse)
import Data.Function ((&))
import qualified Data.Set as Set
import Data.Text (Text, intercalate, lines, unlines)
import Data.Text.IO (readFile)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Polysemy (runM)
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
    shouldMatchList,
  )
import Prelude hiding (lines, readFile, unlines)

spec :: Spec
spec = do
  it "verifies README example" $ do
    teams <- readmeText >>= parseTeamList
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    let groups = currentGroups time teams
    groups
      `shouldMatchList` [ Group
                            { handle = "design-caretaker",
                              description = "Team design caretaker(s)",
                              memberIDs = Set.singleton "U22222BOB"
                            },
                          Group
                            { handle = "design-team",
                              description = "Team design",
                              memberIDs = Set.fromList ["U111ALICE", "U22222BOB", "U333CAROL", "U4444DAVE"]
                            },
                          Group
                            { handle = "dev-caretaker",
                              description = "Team dev caretaker(s)",
                              memberIDs = Set.fromList ["U55555EVE", "U77777GIL"]
                            },
                          Group
                            { handle = "dev-team",
                              description = "Team dev",
                              memberIDs = Set.fromList ["U55555EVE", "U6666FAYE", "U77777GIL", "U88888HAL"]
                            }
                        ]

  it "shows formatted desired team states" $ do
    teams <- readmeText >>= parseTeamList
    time <- iso8601ParseM "2021-10-10T00:00:00Z"
    let states = currentDesiredTeamState time <$> teams

    result <- dryRunExample
    showDesiredTeamStateList mockGetDisplayName states `shouldBe` Just result

parseTeamList :: Text -> IO [Conf]
parseTeamList = runM . runConfig . Config.parse

mockGetDisplayName :: Text -> Maybe Text
mockGetDisplayName = Just . ("@" <>)

readmeText :: IO Text
readmeText = readBlock "haskell" <$> readFile "README.md"

dryRunExample :: IO Text
dryRunExample = skipFirstLine . readBlock "dryRunExample" <$> readFile "README.md"
  where
    skipFirstLine = interUnlines . tail . lines

readBlock :: Text -> Text -> Text
readBlock tag text =
  lines text
    & dropWhile (/= ("```" <> tag))
    & tail
    & takeWhile (/= "```")
    & unlines

interUnlines :: [Text] -> Text
interUnlines = intercalate "\n"
