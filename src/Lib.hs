module Lib
  ( ensure,
    dryRunEnsure,
    listUsers,
    runCanonical,
    CanonicalEffects,
  )
where

import Config
  ( Conf (..),
    Config,
    Effect (..),
    currentResolvedRotationEffects,
    runConfig,
    showResolvedRotationEffectsList,
  )
import qualified Config (parse)
import Control.Category ((>>>))
import Control.Lens ((^.))
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Set (Set)
import Data.Text (Text, pack, unlines)
import Data.Time.Clock (UTCTime)
import Effect.Slack.IO as Slack (apply)
import IO (Env, Time, runEnv, runTime)
import qualified IO as Time (getCurrent)
import Log (runLog)
import qualified Log as Log' (Effects)
import Polysemy (Embed, Final, Member, Members, Sem, embedToFinal, runFinal)
import Polysemy.Error (Error, errorToIOFinal, note)
import Polysemy.Input (Input)
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log (info)
import Slack.Channel as Channel
  ( Channels,
    runChannels,
  )
import qualified Slack.Channel as Channel (Effects)
import Slack.Group as Group
  ( Groups,
    runGroups,
  )
import Slack.User as User
  ( Users,
    displayName,
    id,
    runUsers,
  )
import qualified Slack.User as User (Effects)
import qualified Slack.User as Users (find, listAll)
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
  Log.info "Applying all configurations .."
  traverse_ (applyConf time) confList
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
  showResolvedRotationEffectsList getDisplayName resolvedRotationEffectsList

listUsers :: Members '[Users, Log] r => Sem r Text
listUsers = do
  Log.info "Fetching all users .."
  users <- Users.listAll
  Log.info "Finished fetching all users"
  return $ unlines (formatLine <$> users)
  where
    formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

applyConf ::
  ( Member (Error Text) r,
    Member Channels r,
    Member Users r,
    Member Groups r,
    Member Log r
  ) =>
  UTCTime ->
  Conf ->
  Sem r ()
applyConf time conf = do
  let (members, effects) = currentResolvedRotationEffects time conf
  Log.info "Applying all effects for a rotation .."
  traverse_ (applyEffect members) effects

applyEffect ::
  ( Member (Error Text) r,
    Member Groups r,
    Member Channels r,
    Member Users r,
    Member Log r
  ) =>
  Set Text ->
  Effect ->
  Sem r ()
applyEffect members = withLog \case
  NoOp -> return ()
  Slack effect -> Slack.apply members effect
  where
    withLog ::
      Member Log r =>
      (Effect -> Sem r a) ->
      Effect ->
      Sem r a
    withLog f effect = do
      Log.info (pack (printf "Applying to member IDs %s the effect %s .." (show members) (show effect)))
      result <- f effect
      Log.info (pack (printf "Finished applying effect %s" (show effect)))
      return result

getDisplayName :: Members '[Users, Error Text] r => Text -> Sem r Text
getDisplayName id =
  Users.find id
    >>= note (pack (printf "Could not find user with ID: %s" id))
    <&> (^. User.displayName)
