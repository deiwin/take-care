module ReadmeSpec (spec) where

import Config
  ( Conf,
    currentResolvedRotationEffects,
    runConfig,
    showDryRun,
  )
import qualified Config (parse)
import Data.Function ((&))
import qualified Data.Set as Set
import Data.Text (Text, intercalate, lines, unlines)
import qualified Data.Text as T (takeWhile)
import Data.Text.IO (readFile)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Effect (Effect (..))
import Effect.Slack (SlackEffect (..))
import Polysemy (InterpreterFor, Member, interpret, run, runM)
import Polysemy.Error (Error, runError, throw)
import Slack.User (User (..), Users)
import qualified Slack.User as U (Users (Find, ListAll))
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
      `shouldMatchList` [ ( Set.fromList ["bob@example.com"],
                            [ Slack
                                SetChannelTopic
                                  { name = "tm-design",
                                    topic = \members ->
                                      "Stand-up *9:30*"
                                        <> " :paw_prints: Board :incoming_envelope: https://team.board/url"
                                        <> " :paw_prints: Caretaker "
                                        <> intercalate ", " members
                                  },
                              Slack
                                SetGroup
                                  { handle = "design-caretaker",
                                    name = "Team design caretaker(s)",
                                    channels = []
                                  }
                            ]
                          ),
                          ( Set.fromList ["alice@example.com", "bob@example.com", "carol@example.com", "dave@example.com"],
                            [ Slack
                                SetGroup
                                  { handle = "design-team",
                                    name = "Team design",
                                    channels = ["tm-design"]
                                  }
                            ]
                          ),
                          ( Set.fromList ["eve@example.com", "gil@example.com"],
                            [ Slack
                                SetChannelTopic
                                  { name = "tm-dev",
                                    topic = \members -> intercalate ", " members <> " are the caretakers"
                                  },
                              Slack
                                SetGroup
                                  { handle = "dev-caretaker",
                                    name = "Team dev caretaker(s)",
                                    channels = []
                                  }
                            ]
                          ),
                          ( Set.fromList ["eve@example.com", "faye@example.com", "gil@example.com", "hal@example.com"],
                            [ Slack
                                SetGroup
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
    showDryRun resolvedRotationEffectsList
      & mockRunUser
      & runError
      & run
      & (`shouldBe` Right result)

parseConfList :: Text -> IO [Conf]
parseConfList = runM . runConfig . Config.parse

mockRunUser :: Member (Error Text) r => InterpreterFor Users r
mockRunUser = interpret \case
  U.Find email -> return $ Just $ User {_id = id, _displayName = "@" <> id, _email = email}
    where
      id = T.takeWhile (/= '@') email
  U.ListAll -> throw "Unexpected User.listAll call"

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
