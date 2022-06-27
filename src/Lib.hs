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
import Data.Function ((&))
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unlines)
import Data.Time.Clock (UTCTime)
import IO (Env, Time, runEnv, runTime)
import qualified IO as Time (getCurrent)
import Polysemy (Embed, Final, Member, Sem, embedToFinal, runFinal)
import Polysemy.Error (Error, errorToIOFinal, note)
import Polysemy.Input (Input)
import Slack.Channel as Channel
  ( Channel,
    Channels,
    id,
    runChannels,
  )
import qualified Slack.Channel as Channels (create, find)
import qualified Slack.Channel as Channel (Effects)
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
import qualified Slack.User as Users (listAll)
import Slack.Util (NetCtx, Slack, runNetCtx, runSlack)
import Text.Printf (printf)
import Text.Show.Functions ()
import Prelude hiding (filter, unlines)

newtype GetDisplayName = GetDisplayName
  { unGetDisplayName :: forall r. Member (Error Text) r => Text -> Sem r Text
  }

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)

type CanonicalEffects =
  '[ Time,
     Config
   ]
    ++ Channel.Effects
    ++ '[ Users,
          Groups,
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
  getDisplayName <- getDisplayNameM
  time <- Time.getCurrent
  teamResults <- traverse (wrapTeamResult $ applyConf getDisplayName time) confList
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
  getDisplayName <- getDisplayNameM
  showResolvedRotationEffectsList (unGetDisplayName getDisplayName) resolvedRotationEffectsList

listUsers :: Member Users r => Sem r Text
listUsers = do
  unlines . fmap formatLine <$> Users.listAll
  where
    formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

applyConf ::
  ( Member (Error Text) r,
    Member Channels r,
    Member Groups r
  ) =>
  GetDisplayName ->
  UTCTime ->
  Conf ->
  Sem r ()
applyConf getDisplayName time conf = do
  let (members, effects) = currentResolvedRotationEffects time conf
  traverse_ (applyEffect getDisplayName members) effects

applyEffect ::
  ( Member (Error Text) r,
    Member Groups r,
    Member Channels r
  ) =>
  GetDisplayName ->
  Set Text ->
  Effect ->
  Sem r ()
applyEffect getDisplayName members = \case
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
  _ -> return ()

-- channel <- findOrCreateChannel findChannel $ teamChannelName desiredTeamState
-- ensureChannelTopic getDisplayName channel desiredTeamState
-- let channelID = channel ^. Channel.id
-- traverse_ (ensureGroupState [channelID]) $ groupList desiredTeamState

-- ensureChannelTopic ::
--   ( Member (Error Text) r,
--     Member Channels r
--   ) =>
--   GetDisplayName ->
--   Channel ->
--   DesiredTeamState ->
--   Sem r ()
-- ensureChannelTopic getDisplayName channel desiredTeamState = do
--   newTopic <- topicGivenDisplayNames desiredTeamState (unGetDisplayName getDisplayName)
--   unless (same currentTopic newTopic) $ Channels.setTopic channelID newTopic
--   where
--     channelID = channel ^. Channel.id
--     currentTopic = channel ^. Channel.topic
--     same oldTopic newTopic = oldTopic == newTopic || clean oldTopic == clean newTopic
--     clean = filter (not . potentialAddedChar)
--     potentialAddedChar c = c `elem` ['<', '>']

findOrCreateChannel ::
  Member Channels r =>
  Text ->
  Sem r Channel
findOrCreateChannel name = Channels.find name >>= maybe (Channels.create name) return

getDisplayNameM :: Member Users r => Sem r GetDisplayName
getDisplayNameM = do
  map <- Map.fromList . fmap toPair <$> Users.listAll
  return $ GetDisplayName $ lookup map
  where
    lookup map id = Map.lookup id map & note (pack (printf "Could not find user with ID: %s" id))
    toPair user = (user ^. User.id, user ^. displayName)

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
