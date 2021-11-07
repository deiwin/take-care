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
    showDesiredTeamStateList,
  )
where

import Data.List (cycle)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, intercalate, pack, lines, replicate)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..))
import Dhall (FromDhall)
import qualified Dhall (FromDhall, auto, input)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Prelude hiding (lines, replicate)

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

showDesiredTeamStateList :: forall m. (Monad m) => (Text -> m Text) -> [DesiredTeamState] -> m Text
showDesiredTeamStateList getDisplayName desiredTeamStateList =
  intercalate "\n\n" <$> traverse (showDesiredTeamState getDisplayName) desiredTeamStateList

showDesiredTeamState :: forall m. (Monad m) => (Text -> m Text) -> DesiredTeamState -> m Text
showDesiredTeamState getDisplayName desiredTeamState = interUnlines <$> sequence lines
  where
    lines = titleLine : (padLeft 2 <$$> otherLines)
    titleLine = return "Team design:"
    otherLines = topicLine : groupLines
    topicLine = ("Topic: " <>) <$> topicGivenDisplayNames desiredTeamState getDisplayName
    groupLines = showGroup getDisplayName <$> groupList desiredTeamState

(<$$>) f = fmap $ fmap f

showGroup :: forall m. (Monad m) => (Text -> m Text) -> Group -> m Text
showGroup getDisplayName group = interUnlines <$> sequence lines
  where
    lines = titleLine : (padLeft 2 <$$> otherLines)
    titleLine = return $ pack $ printf "Group @%s:" (handle group)
    otherLines = [descriptionLine, memberLine]
    descriptionLine = return $ "Description: " <> description group
    memberLine = ("Members: " <>) <$> memberNameListText
    memberNameListText = intercalate ", " <$> memberNameList
    memberNameList = traverse getDisplayName (Set.toList $ memberIDs group)

padLeft :: Int -> Text -> Text
padLeft spaces = fmapLines (prefix <>)
  where
    fmapLines f t = interUnlines (f <$> lines t)
    prefix = replicate spaces " "

interUnlines :: [Text] -> Text
interUnlines = intercalate "\n"

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
