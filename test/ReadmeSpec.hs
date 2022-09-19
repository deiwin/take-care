module ReadmeSpec (spec) where

import Config
  ( Conf,
    currentResolvedRotationEffects,
    runConfig,
    showResolvedRotationEffectsList,
  )
import qualified Config (parse)
import Data.Function ((&))
import qualified Data.Set as Set
import Data.Text (Text, intercalate, lines, unlines)
import Data.Text.IO (readFile)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Effect (Effect (..))
import Effect.Slack (SlackEffect (..))
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
    confList <- readmeText >>= parseConfList
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList
    resolvedRotationEffectsList
      `shouldMatchList` [ ( Set.fromList ["U22222BOB"],
                            [ Slack SetChannelTopic
                                { name = "tm-design",
                                  topic = \members ->
                                    "Stand-up *9:30*"
                                      <> " :paw_prints: Board :incoming_envelope: https://team.board/url"
                                      <> " :paw_prints: Caretaker "
                                      <> intercalate ", " members
                                },
                              Slack SetGroup
                                { handle = "design-caretaker",
                                  name = "Team design caretaker(s)",
                                  channels = []
                                }
                            ]
                          ),
                          ( Set.fromList ["U111ALICE", "U22222BOB", "U333CAROL", "U4444DAVE"],
                            [ Slack SetGroup
                                { handle = "design-team",
                                  name = "Team design",
                                  channels = ["tm-design"]
                                }
                            ]
                          ),
                          ( Set.fromList ["U55555EVE", "U77777GIL"],
                            [ Slack SetChannelTopic
                                { name = "tm-dev",
                                  topic = \members -> intercalate ", " members <> " are the caretakers"
                                },
                              Slack SetGroup
                                { handle = "dev-caretaker",
                                  name = "Team dev caretaker(s)",
                                  channels = []
                                }
                            ]
                          ),
                          ( Set.fromList ["U55555EVE", "U6666FAYE", "U77777GIL", "U88888HAL"],
                            [ Slack SetGroup
                                { handle = "dev-team",
                                  name = "Team dev",
                                  channels = ["tm-dev"]
                                }
                            ]
                          )
                        ]

  it "shows formatted desired effects" $ do
    confList <- readmeText >>= parseConfList
    time <- iso8601ParseM "2021-10-10T00:00:00Z"
    let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList

    result <- dryRunExample
    showResolvedRotationEffectsList mockGetDisplayName resolvedRotationEffectsList `shouldBe` Just result

parseConfList :: Text -> IO [Conf]
parseConfList = runM . runConfig . Config.parse

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
