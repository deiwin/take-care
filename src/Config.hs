{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Group (..),
    Conf (..),
    Rotation (..),
    Effect (..),
    DesiredTeamState (..),
    currentGroups,
    currentDesiredTeamState,
    showDesiredTeamStateList,

    -- * Effect
    Config (..),
    runConfig,
    parse,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Set (Set)
import Data.Text (Text, intercalate, lines, pack, replicate)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..))
import Dhall (FromDhall)
import qualified Dhall (auto, input)
import Dhall.TH (HaskellType (..), makeHaskellTypes)
import Effect (Effect (..), SetSlackChannelTopicRecord (..))
import GHC.Generics (Generic)
import Polysemy (Embed, InterpreterFor, Member, embed, interpret, makeSem)
import Text.Printf (printf)
import Prelude hiding (lines, replicate)

Dhall.TH.makeHaskellTypes
  [ MultipleConstructors "Rotation" "./src/Rotation.dhall"
  ]

data Conf = Conf
  { rotation :: Rotation,
    effects :: [Effect]
  }
  deriving (Generic)

instance FromDhall Conf

data Group = Group
  { handle :: Text,
    description :: Text,
    memberIDs :: Set Text
  }
  deriving (Show, Eq)

data DesiredTeamState = DesiredTeamState
  { teamName :: Text,
    teamChannelName :: Text,
    groupList :: [Group],
    topicGivenDisplayNames :: forall m. (Monad m) => (Text -> m Text) -> m Text
  }

data Config m a where
  Parse :: Text -> Config m [Conf]

makeSem ''Config

runConfig :: Member (Embed IO) r => InterpreterFor Config r
runConfig = interpret \case
  Parse input -> embed $ Dhall.input Dhall.auto input

currentGroups :: UTCTime -> [Conf] -> [Group]
-- currentGroups time confList = concat (groupList . currentDesiredTeamState time <$> confList)
currentGroups time confList = undefined

showDesiredTeamStateList :: forall m. (Monad m) => (Text -> m Text) -> [([Text], [Effect])] -> m Text
showDesiredTeamStateList getDisplayName desiredTeamStateList =
  intercalate "\n\n" <$> traverse (showDesiredTeamState getDisplayName) desiredTeamStateList

showDesiredTeamState :: forall m. (Monad m) => (Text -> m Text) -> ([Text], [Effect]) -> m Text
showDesiredTeamState getDisplayName (members, effects) = interUnlines <$> sequence lines
  where
    lines = memberLine : effectLines
    memberLine =
      members
        & traverse getDisplayName
        <&> intercalate ", "
        <&> printf "For %s:"
        <&> pack
    effectLines = padLeft 2 . pack <<$>> showEffect <$> effects
    showEffect = \case
      SetSlackChannelTopic record ->
        members
          & traverse getDisplayName
          <&> topic record
          <&> printf "SetSlackChannelTopic #%s: %s" (name record)
      InviteToSlackChannel name -> return $ printf "InviteToSlackChannel: #%s" name
      SetSlackGroup name -> return $ printf "SetSlackGroup: @%s" name

padLeft :: Int -> Text -> Text
padLeft spaces = fmapLines (prefix <>)
  where
    fmapLines f t = interUnlines (f <$> lines t)
    prefix = replicate spaces " "

interUnlines :: [Text] -> Text
interUnlines = intercalate "\n"

currentDesiredTeamState :: UTCTime -> Conf -> ([Text], [Effect])
currentDesiredTeamState time conf =
  ( resolveRotation $ rotation conf,
    effects conf
  )
  where
    resolveRotation = \case
      Weekly membersList -> currentCaretaker time <$> membersList
      Const members -> members

currentCaretaker :: UTCTime -> [Text] -> Text
currentCaretaker time candidates = cycle candidates !! utcWeek
  where
    (_, utcWeek, _) = toWeekDate $ utctDay time

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
