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
    DesiredTeamState (..),
    Group (..),
    Team (..),
    currentDesiredTeamState,
    runConfig,
    showDesiredTeamStateList,
  )
import qualified Config (parse)
import Control.Category ((>>>))
import Control.Lens ((^.))
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, filter, pack, unlines)
import IO (Env, Time, runEnv, runTime)
import qualified IO as Time (getCurrent)
import Polysemy (Embed, Final, Member, Sem, embedToFinal, runFinal)
import Polysemy.Error (Error, errorToIOFinal, note)
import Polysemy.Input (Input)
import Slack.Channel as Channel
  ( Channel,
    Channels,
    id,
    name,
    runChannels,
    topic,
  )
import qualified Slack.Channel as Channels (create, listAll, setTopic)
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

type FindChannel = Text -> Maybe Channel

type CanonicalEffects =
  '[ Time,
     Config,
     Channels,
     Users,
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
  records <- Config.parse inputText
  getDisplayName <- getDisplayNameM
  findChannel <- findChannelM
  teamResults <- traverse (wrapTeamResult $ ensureTeamState getDisplayName findChannel) records
  return $ unlines teamResults
  where
    wrapTeamResult f record = ("Team " <> team record <> ": success!") <$ f record

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
  desiredStateList <- currentDesiredTeamState time <<$>> Config.parse inputText
  getDisplayName <- getDisplayNameM
  showDesiredTeamStateList (unGetDisplayName getDisplayName) desiredStateList

listUsers :: Member Users r => Sem r Text
listUsers = do
  unlines . fmap formatLine <$> Users.listAll
  where
    formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

ensureTeamState ::
  ( Member Time r,
    Member (Error Text) r,
    Member Channels r,
    Member Groups r
  ) =>
  GetDisplayName ->
  FindChannel ->
  Team ->
  Sem r ()
ensureTeamState getDisplayName findChannel record = do
  time <- Time.getCurrent
  let desiredTeamState = currentDesiredTeamState time record
  channel <- findOrCreateChannel findChannel $ teamChannelName desiredTeamState
  ensureChannelTopic getDisplayName channel desiredTeamState
  let channelID = channel ^. Channel.id
  traverse_ (ensureGroupState [channelID]) $ groupList desiredTeamState

ensureChannelTopic ::
  ( Member (Error Text) r,
    Member Channels r
  ) =>
  GetDisplayName ->
  Channel ->
  DesiredTeamState ->
  Sem r ()
ensureChannelTopic getDisplayName channel desiredTeamState = do
  newTopic <- topicGivenDisplayNames desiredTeamState (unGetDisplayName getDisplayName)
  unless (same currentTopic newTopic) $ Channels.setTopic channelID newTopic
  where
    channelID = channel ^. Channel.id
    currentTopic = channel ^. Channel.topic
    same oldTopic newTopic = oldTopic == newTopic || clean oldTopic == clean newTopic
    clean = filter (not . potentialAddedChar)
    potentialAddedChar c = c `elem` ['<', '>']

findOrCreateChannel ::
  Member Channels r =>
  FindChannel ->
  Text ->
  Sem r Channel
findOrCreateChannel findChannel name = maybe (Channels.create name) return (findChannel name)

ensureGroupState :: Member Groups r => [Text] -> Group -> Sem r ()
ensureGroupState defaultChannelIDs group = do
  existingGroup <- Groups.find $ handle group
  slackGroup <- maybe createNew return existingGroup
  let groupID = slackGroup ^. Group.id

  currentMembers <- Set.fromList <$> Groups.getMembers groupID
  unless (memberIDs group == currentMembers) $ Groups.setMembers groupID $ Set.toList $ memberIDs group

  let currentChannels = slackGroup ^. channelIDs
  unless (same defaultChannelIDs currentChannels) $ Groups.setChannels groupID defaultChannelIDs
  where
    createNew = Groups.create (handle group) (description group) defaultChannelIDs
    same a b = null (a \\ b) && null (b \\ a)

getDisplayNameM :: Member Users r => Sem r GetDisplayName
getDisplayNameM = do
  map <- Map.fromList . fmap toPair <$> Users.listAll
  return $ GetDisplayName $ lookup map
  where
    lookup map id = Map.lookup id map & note (pack (printf "Could not find user with ID: %s" id))
    toPair user = (user ^. User.id, user ^. displayName)

findChannelM :: Member Channels r => Sem r FindChannel
findChannelM = do
  map <- Map.fromList . fmap toPair <$> Channels.listAll
  return (`Map.lookup` map)
  where
    toPair channel = (channel ^. Channel.name, channel)

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
