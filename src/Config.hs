{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Conf (..),
    Rotation (..),
    ResolvedConf,
    resolve,
    apply,
    showDryRun,

    -- * Effect
    Config (..),
    runConfig,
    parse,
  )
where

import Control.Monad (filterM)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Hashable (Hashable (hash))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, intercalate, lines, pack, replicate)
import Data.Time (addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..))
import DeduplicationStore (DeduplicationContext (..), DeduplicationStore (..), isAlreadyApplied, storeAppliedContext)
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

type ResolvedConf = (Set Text, Conf)

runConfig :: Member (Embed IO) r => InterpreterFor Config r
runConfig = interpret \case
  Parse input -> embed $ Dhall.input Dhall.auto input

apply ::
  ( Member (Error Text) r,
    Member Channels r,
    Member Users r,
    Member Groups r,
    Member Log r,
    Member DeduplicationStore r,
    Member Time r
  ) =>
  [ResolvedConf] ->
  Sem r ()
apply = traverse_ (uncurry applyGroup)
  where
    applyGroup members conf@Conf{effects} = do
      Log.info "Applying all effects for a rotation .."
      traverse_ (Effect.apply members) effects
      ctx <- deduplicationContext conf
      maybe (return ()) storeAppliedContext ctx

showDryRun ::
  ( Member (Error Text) r,
    Member Users r
  ) =>
  [ResolvedConf] ->
  Sem r Text
showDryRun resolvedConf =
  resolvedConf
    & traverse showGroup
    <&> intercalate "\n\n"
  where
    showGroup (members, Conf{effects}) = interUnlines <$> sequence lines
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

resolve ::
  ( Member Time r,
    Member Opsgenie r,
    Member Log r,
    Member DeduplicationStore r
  ) =>
  [Conf] ->
  Sem r [ResolvedConf]
resolve confList =
  confList
    & filterM (fmap not . shouldExclude)
    >>= traverse currentResolvedConf
  where
    shouldExclude conf =
      deduplicationContext conf
        >>= maybe (return False) isAlreadyApplied

currentResolvedConf ::
  ( Member Time r,
    Member Opsgenie r,
    Member Log r
  ) =>
  Conf ->
  Sem r ResolvedConf
currentResolvedConf conf = do
  userSet <- Set.fromList <$> resolveRotation (rotation conf)
  return (userSet, conf)

resolveRotation ::
  ( Member Time r,
    Member Opsgenie r,
    Member Log r
  ) =>
  Rotation ->
  Sem r [Text]
resolveRotation = \case
  Weekly membersList -> do
    Log.info (pack (printf "Resolving Weekly rotation for %s .." (show membersList)))
    time <- Time.getCurrent
    let currentMembers = currentCaretaker time <$> membersList
    Log.info (pack (printf "Resolved Weekly rotation. Current members are: %s" (show currentMembers)))
    return currentMembers
  Const members -> do
    Log.info (pack (printf "Resolving Const rotation by returning all listed members: %s .." (show members)))
    return members
  OpsgenieScheduleID scheduleID -> do
    Log.info (pack (printf "Resolving OpsgenieScheduleID rotation for schedule ID %s .." scheduleID))
    currentMembers <- whoIsOnCall scheduleID
    Log.info (pack (printf "Resolved OpsgenieScheduleID rotation. Current members are: %s" (show currentMembers)))
    return currentMembers

deduplicationContext ::
  ( Member Time r
  ) =>
  Conf ->
  Sem r (Maybe DeduplicationContext)
deduplicationContext (Conf {..}) =
  case rotation of
    Const _ -> return Nothing
    OpsgenieScheduleID _ -> return Nothing
    Weekly input -> do
      let effectsHash = pack $ show $ hash effects
      let inputHash = pack $ show $ hash input
      validUntil <- nextWeek <$> Time.getCurrent
      return (Just (DeduplicationContext {..}))

nextWeek :: UTCTime -> UTCTime
nextWeek time = UTCTime newDay 0
  where
    day = utctDay time
    (_, _, weekDay) = toWeekDate day
    -- weekDay is a value between 1 (Monday) and 7 (Sunday). Add however much
    -- is needed to get to the next Monday.
    newDay = addDays (fromIntegral (8 - weekDay)) day

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
