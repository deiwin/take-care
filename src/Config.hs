{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Conf (..),
    Rotation (..),
    ResolvedRotationEffects,
    currentResolvedRotationEffects,
    apply,
    showDryRun,

    -- * Effect
    Config (..),
    runConfig,
    parse,
  )
where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, intercalate, lines, pack, replicate)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..))
import Dhall (FromDhall)
import Dhall qualified (auto, input)
import Dhall.TH (HaskellType (..), makeHaskellTypes)
import Effect (Effect (..))
import Effect qualified (apply, showDryRun)
import GHC.Generics (Generic)
import IO (Time)
import IO qualified as Time (getCurrent)
import Opsgenie (Opsgenie, whoIsOnCall)
import Polysemy (Embed, InterpreterFor, Member, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error)
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log (info)
import Slack.Channel (Channels)
import Slack.Group (Groups)
import Slack.User (Users)
import Text.Printf (printf)
import Text.Show.Functions ()
import Prelude hiding (filter, lines, replicate, unlines)

Dhall.TH.makeHaskellTypes
  [ MultipleConstructors "Rotation" "./types/core/Rotation.dhall"
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

apply ::
  ( Member (Error Text) r,
    Member Channels r,
    Member Users r,
    Member Groups r,
    Member Log r
  ) =>
  [(Set Text, [Effect])] ->
  Sem r ()
apply = traverse_ (uncurry applyGroup)
  where
    applyGroup members effects = do
      Log.info "Applying all effects for a rotation .."
      traverse_ (Effect.apply members) effects

showDryRun ::
  ( Member (Error Text) r,
    Member Users r
  ) =>
  [(Set Text, [Effect])] ->
  Sem r Text
showDryRun resolvedRotationEffectsList =
  resolvedRotationEffectsList
    & traverse showGroup
    <&> intercalate "\n\n"
  where
    showGroup (members, effects) = interUnlines <$> sequence lines
      where
        lines = memberLine : effectLines
        memberLine =
          members
            & Set.toList
            & intercalate ", "
            & printf "For %s:"
            & pack
            & return
        effectLines = padLeft 2 <<$>> Effect.showDryRun members <$> effects

currentResolvedRotationEffects ::
  ( Member Time r,
    Member Opsgenie r
  ) =>
  Conf ->
  Sem r ResolvedRotationEffects
currentResolvedRotationEffects conf = do
  userSet <- Set.fromList <$> resolveRotation (rotation conf)
  return (userSet, effects conf)

resolveRotation ::
  ( Member Time r,
    Member Opsgenie r
  ) =>
  Rotation ->
  Sem r [Text]
resolveRotation = \case
  Weekly membersList -> do
    time <- Time.getCurrent
    return $ currentCaretaker time <$> membersList
  Const members -> return members
  OpsgenieScheduleID scheduleID -> whoIsOnCall scheduleID

currentCaretaker :: UTCTime -> [Text] -> Text
currentCaretaker time candidates = cycle candidates !! utcWeek
  where
    (_, utcWeek, _) = toWeekDate $ utctDay time

padLeft :: Int -> Text -> Text
padLeft spaces = fmapLines (prefix <>)
  where
    fmapLines f t = interUnlines (f <$> lines t)
    prefix = replicate spaces " "

interUnlines :: [Text] -> Text
interUnlines = intercalate "\n"

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
