{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

import Config
  ( DesiredTeamState (..),
    Group (..),
    Members (..),
    Team (..),
    currentDesiredTeamState,
    currentGroups,
    parseTeamList,
  )
import Control.Exception (evaluate)
import Data.Maybe (Maybe (..), fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import NeatInterpolation (trimming)
import Test.Hspec (anyException, describe, hspec, it, shouldBe, shouldMatchList, shouldThrow)

main :: IO ()
main = hspec $ do
  describe "Config" $ do
    it "verifies README example" $ do
      teams <-
        parseTeamList
          [trimming|
              [ { members = { caretakers = [[ "U111ALICE" -- Alice
                                            , "U22222BOB" -- Bob
                                            , "U333CAROL" -- Carol
                                            ]
                                           ]
                            , others = [ "U4444DAVE" -- Dave
                                       ]
                            }
                , team = "design"
                , topic = \(caretaker : Text) ->
                     let standup   = "Stand-up *9:30*"
                  in let board     = "Board :incoming_envelope: https://team.board/url"
                  in let separator = ":paw_prints:"
                  in "$${standup} $${separator} $${board} $${separator} Caretaker $${caretaker}"
                }
              , { members = { caretakers = [[ "U55555EVE" -- Eve
                                            , "U6666FAYE" -- Faye
                                            ]
                                           ,[ "U77777GIL" -- Gil
                                            , "U88888HAL" -- Hal
                                            ]
                                           ]
                            , others = [] : List Text
                            }
                , team = "dev"
                , topic = \(caretakers : Text) -> "$${caretakers} are the caretakers"
                }
              ]
            |]
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

    it "shows correct topic given display names for IDs" $ do
      team <-
        head
          <$> parseTeamList
            [trimming|
              [ { members = { caretakers = [[ "U55555EVE" -- Eve
                                            , "U6666FAYE" -- Faye
                                            ]
                                           ,[ "U77777GIL" -- Gil
                                            , "U88888HAL" -- Hal
                                            ]
                                           ]
                            , others = [] : List Text
                            }
                , team = "design"
                , topic = \(caretaker : Text) ->
                     let standup   = "Stand-up *9:30*"
                  in let board     = "Board :incoming_envelope: https://team.board/url"
                  in let separator = ":paw_prints:"
                  in "$${standup} $${separator} $${board} $${separator} Caretaker $${caretaker}"
                }
              ]
            |]
      time <- iso8601ParseM "2021-10-10T00:00:00Z"

      let state = currentDesiredTeamState time team

      mockShowTopic (topicGivenDisplayNames state)
        `shouldBe` "Stand-up *9:30* :paw_prints: \
                   \Board :incoming_envelope: https://team.board/url :paw_prints: \
                   \Caretaker @U55555EVE, @U77777GIL"

mockShowTopic :: (forall m. (Monad m) => (Text -> m Text) -> m Text) -> Text
mockShowTopic f = fromJust $ f (Just . ("@" <>))
