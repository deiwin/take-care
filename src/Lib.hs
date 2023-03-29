module Lib
  ( ensure,
    dryRunEnsure,
    listUsers,
    runCanonical,
    CanonicalEffects,
  )
where

import Config
  ( Config,
    apply,
    currentResolvedRotationEffects,
    runConfig,
    showDryRun,
  )
import Config qualified (parse)
import Control.Category ((>>>))
import Control.Lens ((^.))
import Data.Text (Text, pack, unlines)
import IO (Env, Time, runEnv, runTime)
import IO qualified as Time (getCurrent)
import Log (runLog)
import Log qualified as Log' (Effects)
import Polysemy (Embed, Final, Member, Members, Sem, embedToFinal, runFinal)
import Polysemy.Error (Error, errorToIOFinal)
import Polysemy.Input (Input)
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log (info)
import Slack.Channel as Channel (Channels, runChannels)
import Slack.Channel qualified as Channel (Effects)
import Slack.Group as Group (Groups, runGroups)
import Slack.User as User (Users, displayName, email, runUsers)
import Slack.User qualified as User (Effects)
import Slack.User qualified as Users (listAll)
import Slack.Util (NetCtx, Slack, runNetCtx, runSlack)
import Text.Printf (printf)
import Text.Show.Functions ()
import Prelude hiding (filter, unlines)

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)

type CanonicalEffects =
  '[ Time,
     Config
   ]
    ++ Channel.Effects
    ++ User.Effects
    ++ '[ Groups,
          Slack,
          Input NetCtx,
          Env,
          Error Text
        ]
    ++ Log'.Effects
    ++ '[ Embed IO,
          Final IO
        ]

runCanonical :: Sem CanonicalEffects a -> IO (Either Text a)
runCanonical =
  runTime
    >>> runConfig
    >>> runChannels
    >>> runUsers
    >>> runGroups
    >>> runSlack
    >>> runNetCtx
    >>> runEnv
    >>> errorToIOFinal
    >>> runLog
    >>> embedToFinal @IO
    >>> runFinal @IO

ensure ::
  ( Member Config r,
    Member Time r,
    Member (Error Text) r,
    Member Channels r,
    Member Users r,
    Member Groups r,
    Member Log r
  ) =>
  Text ->
  Sem r ()
ensure inputText = do
  Log.info "Parsing configuration .."
  confList <- Config.parse inputText
  Log.info "Resolving current time .."
  time <- Time.getCurrent
  Log.info "Resolving rotation effects .."
  let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList
  Log.info "Applying all configurations .."
  apply resolvedRotationEffectsList
  Log.info "Completed applying all configurations"

dryRunEnsure ::
  ( Member Config r,
    Member Time r,
    Member (Error Text) r,
    Member Users r,
    Member Log r
  ) =>
  Text ->
  Sem r Text
dryRunEnsure inputText = do
  Log.info "Parsing configuration .."
  confList <- Config.parse inputText
  Log.info "Resolving current time .."
  time <- Time.getCurrent
  Log.info "Resolving rotation effects .."
  let resolvedRotationEffectsList = currentResolvedRotationEffects time <$> confList
  Log.info "Showing resolved rotation effects .."
  showDryRun resolvedRotationEffectsList

listUsers :: Members '[Users, Log] r => Sem r Text
listUsers = do
  Log.info "Fetching all users .."
  users <- Users.listAll
  Log.info "Finished fetching all users"
  return $ unlines (formatLine <$> users)
  where
    formatLine user = pack $ printf "%s: %s" (user ^. email) (user ^. displayName)
