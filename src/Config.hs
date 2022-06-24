{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Conf (..),
    Rotation (..),
    Effect (..),
    ResolvedRotationEffects,
    currentResolvedRotationEffects,
    showResolvedRotationEffectsList,

    -- * Effect
    Config (..),
    runConfig,
    parse,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text, intercalate, lines, pack, replicate)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..))
import Dhall (FromDhall)
import qualified Dhall (auto, input)
import Data.Set (Set)
import qualified Data.Set as Set
import Dhall.TH (HaskellType (..), makeHaskellTypes)
import Effect (Effect (..))
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

data Config m a where
  Parse :: Text -> Config m [Conf]

makeSem ''Config

type ResolvedRotationEffects = (Set Text, [Effect])

runConfig :: Member (Embed IO) r => InterpreterFor Config r
runConfig = interpret \case
  Parse input -> embed $ Dhall.input Dhall.auto input

showResolvedRotationEffectsList :: forall m. (Monad m) => (Text -> m Text) -> [ResolvedRotationEffects] -> m Text
showResolvedRotationEffectsList getDisplayName resolvedRotationEffectsList =
  resolvedRotationEffectsList
    & traverse (showResolvedRotationEffects getDisplayName)
    <&> intercalate "\n\n"

showResolvedRotationEffects :: forall m. (Monad m) => (Text -> m Text) -> ResolvedRotationEffects -> m Text
showResolvedRotationEffects getDisplayName (members, effects) = interUnlines <$> sequence lines
  where
    lines = memberLine : effectLines
    memberLine =
      members
        & Set.toList
        & traverse getDisplayName
        <&> intercalate ", "
        <&> printf "For %s:"
        <&> pack
    effectLines = padLeft 2 . pack <<$>> showEffect <$> effects
    showEffect = \case
      record@SetSlackChannelTopic{} ->
        members
          & Set.toList
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

currentResolvedRotationEffects :: UTCTime -> Conf -> ResolvedRotationEffects
currentResolvedRotationEffects time conf =
  ( Set.fromList $ resolveRotation $ rotation conf,
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
