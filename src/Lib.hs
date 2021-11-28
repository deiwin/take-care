module Lib
  ( ensure,
    dryRunEnsure,
    listUsers,
    runCanonical,
    CanonicalEffects,
  )
where

import Config
  ( DesiredTeamState (..),
    Group (..),
    Team (..),
    currentDesiredTeamState,
    parseTeamList,
    showDesiredTeamStateList,
  )
import Control.Category ((>>>))
import Control.Lens ((^.))
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, filter, pack, unlines)
import Data.Time.Clock (getCurrentTime)
import IO (Env, runEnv)
import Polysemy (Embed, Final, Member, Sem, embedFinal, embedToFinal, runFinal)
import Polysemy.Error (Error, errorToIOFinal, note)
import Polysemy.View (View (..))
import Slack.Channel as Channel
  ( Channel,
    Channels,
    create,
    find,
    id,
    runChannels,
    setTopic,
    topic,
  )
import Slack.Group as Group
  ( Groups,
    channelIDs,
    create,
    find,
    getMembers,
    id,
    runGroups,
    setChannels,
    setMembers,
  )
import Slack.User as User
  ( Users,
    displayName,
    id,
    listAll,
    runUsers,
  )
import Slack.Util (NetCtx, runNetCtx)
import Text.Printf (printf)
import Text.Show.Functions ()
import Prelude hiding (filter, unlines)

newtype GetDisplayName = GetDisplayName
  { unGetDisplayName :: forall r. Member (Error Text) r => Text -> Sem r Text
  }

type CanonicalEffects =
  '[ Channels,
     Users,
     Groups,
     View NetCtx,
     Env,
     Error Text,
     Embed IO,
     Final IO
   ]

runCanonical :: Sem CanonicalEffects Text -> IO (Either Text Text)
runCanonical =
  runChannels
    >>> runUsers
    >>> runGroups
    >>> runNetCtx
    >>> runEnv
    >>> errorToIOFinal
    >>> embedToFinal @IO
    >>> runFinal

ensure ::
  ( Member (Final IO) r,
    Member (Error Text) r,
    Member Channels r,
    Member Users r,
    Member Groups r
  ) =>
  Text ->
  Sem r Text
ensure inputText = do
  records <- embedFinal $ parseTeamList inputText
  getDisplayName <- getDisplayNameM
  teamResults <- traverse (wrapTeamResult $ ensureTeamState getDisplayName) records
  return $ unlines teamResults
  where
    wrapTeamResult f record = ("Team " <> team record <> ": success!") <$ f record

dryRunEnsure ::
  ( Member (Final IO) r,
    Member (Error Text) r,
    Member Users r
  ) =>
  Text ->
  Sem r Text
dryRunEnsure inputText = do
  time <- embedFinal getCurrentTime
  desiredStateList <- embedFinal $ currentDesiredTeamState time <<$>> parseTeamList inputText
  getDisplayName <- getDisplayNameM
  showDesiredTeamStateList (unGetDisplayName getDisplayName) desiredStateList

listUsers :: Member Users r => Sem r Text
listUsers = do
  unlines . fmap formatLine <$> User.listAll
  where
    formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

ensureTeamState ::
  ( Member (Final IO) r,
    Member (Error Text) r,
    Member Channels r,
    Member Groups r
  ) =>
  GetDisplayName ->
  Team ->
  Sem r ()
ensureTeamState getDisplayName record = do
  time <- embedFinal getCurrentTime
  let desiredTeamState = currentDesiredTeamState time record
  channel <- findOrCreateChannel $ teamChannelName desiredTeamState
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
  unless (same currentTopic newTopic) $ setTopic channelID newTopic
  where
    channelID = channel ^. Channel.id
    currentTopic = channel ^. Channel.topic
    same oldTopic newTopic = oldTopic == newTopic || clean oldTopic == clean newTopic
    clean = filter (not . potentialAddedChar)
    potentialAddedChar c = c `elem` ['<', '>']

findOrCreateChannel :: Member Channels r => Text -> Sem r Channel
findOrCreateChannel name = do
  current <- Channel.find name
  maybe (Channel.create name) return current

ensureGroupState :: Member Groups r => [Text] -> Group -> Sem r ()
ensureGroupState defaultChannelIDs group = do
  existingGroup <- Group.find $ handle group
  slackGroup <- maybe createNew return existingGroup
  let groupID = slackGroup ^. Group.id

  currentMembers <- Set.fromList <$> Group.getMembers groupID
  unless (memberIDs group == currentMembers) $ Group.setMembers groupID $ Set.toList $ memberIDs group

  let currentChannels = slackGroup ^. channelIDs
  unless (same defaultChannelIDs currentChannels) $ Group.setChannels groupID defaultChannelIDs
  where
    createNew = Group.create (handle group) (description group) defaultChannelIDs
    same a b = null (a \\ b) && null (b \\ a)

getDisplayNameM :: Member Users r => Sem r GetDisplayName
getDisplayNameM = do
  map <- Map.fromList . fmap toPair <$> User.listAll
  return $ GetDisplayName $ lookup map
  where
    lookup map id = Map.lookup id map & note (pack (printf "Could not find user with ID: %s" id))
    toPair user = (user ^. User.id, user ^. displayName)

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
