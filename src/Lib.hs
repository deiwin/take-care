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
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, filter, pack, unlines)
import Data.Time.Clock (UTCTime)
import IO (Env, Time, runEnv, runTime)
import qualified IO as Time (getCurrent)
import Polysemy (Embed, Final, Member, Members, Sem, embedToFinal, runFinal)
import Polysemy.Error (Error, errorToIOFinal, note)
import Polysemy.Input (Input)
import Slack.Channel as Channel
  ( Channel,
    Channels,
    id,
    runChannels,
  )
import qualified Slack.Channel as Channel (Effects, topic)
import qualified Slack.Channel as Channels (create, find, setTopic)
import Slack.Group as Group
  ( Groups,
    channelIDs,
    id,
    runGroups,
  )
import qualified Slack.Group as Groups
  ( create,
    find,
    getMembers,
    setChannels,
    setMembers,
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
          Error Text,
          Embed IO,
          Final IO
        ]

runCanonical :: Sem CanonicalEffects Text -> IO (Either Text Text)
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
    >>> embedToFinal @IO
    >>> runFinal @IO

ensure ::
  ( Member Config r,
    Member Time r,
    Member (Error Text) r,
    Member Channels r,
    Member Users r,
    Member Groups r
  ) =>
  Text ->
  Sem r Text
ensure inputText = do
  confList <- Config.parse inputText
  time <- Time.getCurrent
  teamResults <- traverse (wrapTeamResult $ applyConf time) confList
  return $ unlines teamResults
  where
    wrapTeamResult f record = "Team MOCK: success!" <$ f record

dryRunEnsure ::
  ( Member Config r,
    Member Time r,
    Member (Error Text) r,
    Member Users r
  ) =>
  Text ->
  Sem r Text
dryRunEnsure inputText = do
  time <- Time.getCurrent
  resolvedRotationEffectsList <- currentResolvedRotationEffects time <<$>> Config.parse inputText
  showResolvedRotationEffectsList getDisplayName resolvedRotationEffectsList

listUsers :: Member Users r => Sem r Text
listUsers = do
  unlines . fmap formatLine <$> Users.listAll
  where
    formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

applyConf ::
  ( Member (Error Text) r,
    Member Channels r,
    Member Users r,
    Member Groups r
  ) =>
  UTCTime ->
  Conf ->
  Sem r ()
applyConf time conf = do
  let (members, effects) = currentResolvedRotationEffects time conf
  traverse_ (applyEffect members) effects

applyEffect ::
  ( Member (Error Text) r,
    Member Groups r,
    Member Channels r,
    Member Users r
  ) =>
  Set Text ->
  Effect ->
  Sem r ()
applyEffect members = \case
  SetSlackGroup {handle, name, channels} -> do
    existingGroup <- Groups.find handle
    defaultChannelIDs <- (^. Channel.id) <<$>> traverse findOrCreateChannel channels
    slackGroup <- maybe (createNew defaultChannelIDs) return existingGroup
    let groupID = slackGroup ^. Group.id

    currentMembers <- Set.fromList <$> Groups.getMembers groupID
    unless (members == currentMembers) $ Groups.setMembers groupID $ Set.toList members

    let currentChannels = slackGroup ^. channelIDs
    unless (same defaultChannelIDs currentChannels) $ Groups.setChannels groupID defaultChannelIDs
    where
      createNew = Groups.create handle name
      same a b = null (a \\ b) && null (b \\ a)
  SetSlackChannelTopic {name, topic} -> do
    channel <- findOrCreateChannel name
    newTopic <- topic <$> traverse getDisplayName (Set.toList members)
    unless
      (same (channel ^. Channel.topic) newTopic)
      (Channels.setTopic (channel ^. Channel.id) newTopic)
    where
      same oldTopic newTopic = oldTopic == newTopic || clean oldTopic == clean newTopic
      clean = filter (not . potentialAddedChar)
      potentialAddedChar c = c `elem` ['<', '>']

getDisplayName :: Members '[Users, Error Text] r => Text -> Sem r Text
getDisplayName id =
  Users.find id
    >>= note (pack (printf "Could not find user with ID: %s" id))
    <&> (^. User.displayName)

findOrCreateChannel ::
  Member Channels r =>
  Text ->
  Sem r Channel
findOrCreateChannel name = Channels.find name >>= maybe (Channels.create name) return

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
