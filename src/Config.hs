{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Conf (..),
    Rotation (..),
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, intercalate, lines, pack, replicate)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..))
import Dhall (FromDhall)
import qualified Dhall (auto, input)
import Dhall.TH (HaskellType (..), makeHaskellTypes)
import Effect (Effect (..))
import qualified Effect.Slack.IO as Slack (showDryRun)
import GHC.Generics (Generic)
import Polysemy (Embed, InterpreterFor, Member, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error)
import Slack.User as User (Users)
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

showResolvedRotationEffectsList ::
  ( Member (Error Text) r,
    Member Users r
  ) =>
  [ResolvedRotationEffects] ->
  Sem r Text
showResolvedRotationEffectsList resolvedRotationEffectsList =
  resolvedRotationEffectsList
    & traverse showResolvedRotationEffects
    <&> intercalate "\n\n"

showResolvedRotationEffects ::
  ( Member (Error Text) r,
    Member Users r
  ) =>
  ResolvedRotationEffects ->
  Sem r Text
showResolvedRotationEffects (members, effects) = interUnlines <$> sequence lines
  where
    lines = memberLine : effectLines
    memberLine =
      members
        & Set.toList
        & intercalate ", "
        & printf "For %s:"
        & pack
        & return
    effectLines = padLeft 2 <<$>> showEffect <$> effects
    showEffect = \case
      NoOp -> return "NoOp"
      Slack effect -> Slack.showDryRun members effect

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
