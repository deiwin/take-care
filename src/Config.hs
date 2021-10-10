{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config
  ( Members (..),
    Team (..),
    Group (..),
    DesiredTeamState (..),
    parseTeamList,
    currentGroups,
    currentDesiredTeamState,
  )
where

import Data.List (cycle, elem, nub, null, zip3, (\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, filter, intercalate, pack, unlines)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..))
import Dhall (FromDhall)
import qualified Dhall (FromDhall, auto, input)
import GHC.Generics (Generic)

data Members = Members
  { caretakers :: [[Text]],
    others :: [Text]
  }
  deriving (Generic, Show)

instance FromDhall Members

data Team = Team
  { members :: Members,
    team :: Text, -- TODO should be max 21 chars with the tm- prefix, so 18
    topic :: Text -> Text
  }
  deriving (Generic)

instance FromDhall Team

data Group = Group
  { handle :: Text,
    description :: Text,
    memberIDs :: Set Text
  }
  deriving (Show, Eq)

data DesiredTeamState = DesiredTeamState
  { groupList :: [Group],
    topicGivenDisplayNames :: forall m. (Monad m) => (Text -> m Text) -> m Text
  }

parseTeamList :: Text -> IO [Team]
parseTeamList = Dhall.input Dhall.auto

parseTeam :: Text -> IO Team
parseTeam = Dhall.input Dhall.auto

currentGroups :: UTCTime -> [Team] -> [Group]
currentGroups time teams = concat (groupList . currentDesiredTeamState time <$> teams)

currentDesiredTeamState :: UTCTime -> Team -> DesiredTeamState
currentDesiredTeamState time team =
  DesiredTeamState
    { groupList = [caretakers, everyone],
      topicGivenDisplayNames =
        \getDisplayName -> do
          displayNameList <- traverse getDisplayName $ Set.toList caretakerIDs
          return $ topic team $ intercalate ", " displayNameList
    }
  where
    caretakers =
      Group
        { handle = teamName <> "-caretaker",
          description = "Team " <> teamName <> " caretaker(s)",
          memberIDs = caretakerIDs
        }
    caretakerIDs = Set.fromList $ currentCaretakerList time team
    everyone =
      Group
        { handle = teamName <> "-team",
          description = "Team " <> teamName,
          memberIDs = Set.fromList $ others (members team) ++ concat (Config.caretakers $ members team)
        }
    teamName = Config.team team

currentCaretakerList :: UTCTime -> Team -> [Text]
currentCaretakerList time team = currentCaretaker time <$> caretakers (members team)

currentCaretaker :: UTCTime -> [Text] -> Text
currentCaretaker time candidates = cycle candidates !! utcWeek
  where
    (_, utcWeek, _) = toWeekDate $ utctDay time
