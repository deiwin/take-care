{-# LANGUAGE FlexibleInstances #-}

module Effect
  ( Effect (..),
    apply,
    showDryRun,
  )
where

import Data.Set (Set)
import Data.Text (Text, pack)
import Dhall (FromDhall)
import Effect.Slack (SlackEffect (..))
import qualified Effect.Slack.IO as Slack (apply, showDryRun)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error)
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log (info)
import Slack.Channel as Channel (Channels)
import Slack.Group as Group (Groups)
import Slack.User as User (Users)
import Text.Printf (printf)
import Text.Show.Functions ()
import Prelude hiding (filter, lines, replicate, unlines)

data Effect
  = Slack SlackEffect
  | NoOp -- Here to force a union type
  deriving (Generic, Show, Eq)

instance FromDhall Effect

apply ::
  ( Member (Error Text) r,
    Member Groups r,
    Member Channels r,
    Member Users r,
    Member Log r
  ) =>
  Set Text ->
  Effect ->
  Sem r ()
apply members = withLog \case
  NoOp -> return ()
  Slack effect -> Slack.apply members effect
  where
    withLog ::
      Member Log r =>
      (Effect -> Sem r a) ->
      Effect ->
      Sem r a
    withLog f effect = do
      Log.info (pack (printf "Applying to members %s the effect %s .." (show members) (show effect)))
      result <- f effect
      Log.info (pack (printf "Finished applying effect %s" (show effect)))
      return result

showDryRun ::
  ( Member (Error Text) r,
    Member Users r
  ) =>
  Set Text ->
  Effect ->
  Sem r Text
showDryRun members = \case
  NoOp -> return "NoOp"
  Slack effect -> Slack.showDryRun members effect
