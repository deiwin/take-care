module ReadmeSpec (spec) where

import Config
  ( Conf,
    Effect (..),
    currentResolvedRotationEffects,
    runConfig,
    showResolvedRotationEffectsList,
  )
import qualified Config (parse)
import Data.Function ((&))
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
    confList <- readmeText >>= parseConfList
    time <- iso8601ParseM "2021-10-10T00:00:00Z"

    let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList
    resolvedRotationEffectsList
      `shouldMatchList` [ ( ["U22222BOB"],
                            [ SetSlackChannelTopic
                                { name = "tm-design",
                                  topic = \members ->
                                    "Stand-up *9:30*"
                                      <> " :paw_prints: Board :incoming_envelope: https://team.board/url"
                                      <> " :paw_prints: Caretaker "
                                      <> intercalate ", " members
                                },
                              SetSlackGroup "design-caretaker"
                            ]
                          ),
                          ( ["U111ALICE", "U22222BOB", "U333CAROL", "U4444DAVE"],
                            [ InviteToSlackChannel "tm-design",
                              SetSlackGroup "design-team"
                            ]
                          ),
                          ( ["U55555EVE", "U77777GIL"],
                            [ SetSlackChannelTopic
                                { name = "tm-dev",
                                  topic = \members -> intercalate ", " members <> " are the caretakers"
                                },
                              SetSlackGroup "dev-caretaker"
                            ]
                          ),
                          ( ["U55555EVE", "U6666FAYE", "U77777GIL", "U88888HAL"],
                            [ InviteToSlackChannel "tm-dev",
                              SetSlackGroup "dev-team"
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
