module Lib
  ( ensure,
    dryRunEnsure,
    listUsers,
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
import Control.Error.Util ((??))
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, filter, pack, unlines)
import Data.Time.Clock (getCurrentTime)
import Network.Wreq.Session (newAPISession)
import Polysemy (Final, Member, Sem, embedFinal)
import Slack.Channel as Channel
  ( Channel,
    createChannel,
    findChannel,
    id,
    setChannelTopic,
    topic,
  )
import Slack.Group as Group
  ( channelIDs,
    createGroup,
    findGroup,
    getGroupMembers,
    id,
    setGroupChannels,
    setGroupMembers,
  )
import Slack.User as User (displayName, id, listAllUsers)
import Slack.Util (NetCtx (..))
import Text.Printf (printf)
import Text.Show.Functions ()
import Prelude hiding (filter, unlines)

newtype GetDisplayName = GetDisplayName
  { unGetDisplayName :: forall r. Member (Final IO) r => Text -> ExceptT Text (Sem r) Text
  }

ensure :: Member (Final IO) r => Text -> ByteString -> ExceptT Text (Sem r) Text
ensure inputText apiToken = do
  netCtx <- lift $ embedFinal (NetCtx apiToken <$> newAPISession)
  records <- lift $ embedFinal $ parseTeamList inputText
  getDisplayName <- getDisplayNameM netCtx
  teamResults <- traverse (wrapTeamResult $ ensureTeamState netCtx getDisplayName) records
  return $ unlines teamResults
  where
    wrapTeamResult f record = ("Team " <> team record <> ": success!") <$ f record

dryRunEnsure :: Member (Final IO) r => Text -> ByteString -> ExceptT Text (Sem r) Text
dryRunEnsure inputText apiToken = do
  time <- lift $ embedFinal getCurrentTime
  desiredStateList <- lift $ embedFinal $ currentDesiredTeamState time <<$>> parseTeamList inputText
  netCtx <- lift $ embedFinal (NetCtx apiToken <$> newAPISession)
  getDisplayName <- getDisplayNameM netCtx
  showDesiredTeamStateList (unGetDisplayName getDisplayName) desiredStateList

listUsers :: Member (Final IO) r => ByteString -> ExceptT Text (Sem r) Text
listUsers apiToken = do
  netCtx <- lift $ embedFinal (NetCtx apiToken <$> newAPISession)
  unlines . fmap formatLine <$> listAllUsers netCtx
  where
    formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

ensureTeamState :: Member (Final IO) r => NetCtx -> GetDisplayName -> Team -> ExceptT Text (Sem r) ()
ensureTeamState netCtx getDisplayName record = do
  time <- lift $ embedFinal getCurrentTime
  let desiredTeamState = currentDesiredTeamState time record
  channel <- findOrCreateChannel netCtx $ teamChannelName desiredTeamState
  ensureChannelTopic netCtx getDisplayName channel desiredTeamState
  let channelID = channel ^. Channel.id
  traverse_ (ensureGroupState netCtx [channelID]) $ groupList desiredTeamState

ensureChannelTopic ::
  Member (Final IO) r =>
  NetCtx ->
  GetDisplayName ->
  Channel ->
  DesiredTeamState ->
  ExceptT Text (Sem r) ()
ensureChannelTopic netCtx getDisplayName channel desiredTeamState = do
  newTopic <- topicGivenDisplayNames desiredTeamState (unGetDisplayName getDisplayName)
  unless (same currentTopic newTopic) $ setChannelTopic netCtx channelID newTopic
  where
    channelID = channel ^. Channel.id
    currentTopic = channel ^. Channel.topic
    same oldTopic newTopic = oldTopic == newTopic || clean oldTopic == clean newTopic
    clean = filter (not . potentialAddedChar)
    potentialAddedChar c = c `elem` ['<', '>']

findOrCreateChannel :: Member (Final IO) r => NetCtx -> Text -> ExceptT Text (Sem r) Channel
findOrCreateChannel netCtx name = do
  current <- findChannel netCtx name
  maybe (createChannel netCtx name) return current

ensureGroupState :: Member (Final IO) r => NetCtx -> [Text] -> Group -> ExceptT Text (Sem r) ()
ensureGroupState netCtx defaultChannelIDs group = do
  existingGroup <- findGroup netCtx $ handle group
  slackGroup <- maybe createNew return existingGroup
  let groupID = slackGroup ^. Group.id

  currentMembers <- Set.fromList <$> getGroupMembers netCtx groupID
  unless (memberIDs group == currentMembers) $ setGroupMembers netCtx groupID $ Set.toList $ memberIDs group

  let currentChannels = slackGroup ^. channelIDs
  unless (same defaultChannelIDs currentChannels) $ setGroupChannels netCtx groupID defaultChannelIDs
  where
    createNew = createGroup netCtx (handle group) (description group) defaultChannelIDs
    same a b = null (a \\ b) && null (b \\ a)

getDisplayNameM :: Member (Final IO) r => NetCtx -> ExceptT Text (Sem r) GetDisplayName
getDisplayNameM netCtx = do
  map <- Map.fromList . fmap toPair <$> listAllUsers netCtx
  return $ GetDisplayName $ lookup map
  where
    lookup map id = Map.lookup id map ?? pack (printf "Could not find user with ID: %s" id)
    toPair user = (user ^. User.id, user ^. displayName)

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
