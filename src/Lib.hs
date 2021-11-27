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
import Control.Error.Util ((??))
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, filter, pack, unlines)
import Data.Time.Clock (getCurrentTime)
import Network.Wreq.Session (newAPISession)
import Polysemy (Embed, Final, Member, Sem, embed, embedFinal, embedToFinal, interpret, runFinal)
import Polysemy.View (View (..))
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

type CanonicalEffects =
  '[ View NetCtx,
     Embed IO,
     Final IO
   ]

runNetCtx :: Member (Embed IO) r => ByteString -> Sem (View NetCtx ': r) a -> Sem r a
runNetCtx token program = do
  session <- embed newAPISession
  interpret
    ( \case
        See -> return (NetCtx token session)
    )
    program

runCanonical :: ByteString -> ExceptT Text (Sem CanonicalEffects) Text -> IO (Either Text Text)
runCanonical token program = do
  program
    & runExceptT
    & runNetCtx token
    & embedToFinal @IO
    & runFinal

ensure ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  Text ->
  ExceptT Text (Sem r) Text
ensure inputText = do
  records <- lift $ embedFinal $ parseTeamList inputText
  getDisplayName <- getDisplayNameM
  teamResults <- traverse (wrapTeamResult $ ensureTeamState getDisplayName) records
  return $ unlines teamResults
  where
    wrapTeamResult f record = ("Team " <> team record <> ": success!") <$ f record

dryRunEnsure ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  Text ->
  ExceptT Text (Sem r) Text
dryRunEnsure inputText = do
  time <- lift $ embedFinal getCurrentTime
  desiredStateList <- lift $ embedFinal $ currentDesiredTeamState time <<$>> parseTeamList inputText
  getDisplayName <- getDisplayNameM
  showDesiredTeamStateList (unGetDisplayName getDisplayName) desiredStateList

listUsers ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  ExceptT Text (Sem r) Text
listUsers = do
  unlines . fmap formatLine <$> listAllUsers
  where
    formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

ensureTeamState ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  GetDisplayName ->
  Team ->
  ExceptT Text (Sem r) ()
ensureTeamState getDisplayName record = do
  time <- lift $ embedFinal getCurrentTime
  let desiredTeamState = currentDesiredTeamState time record
  channel <- findOrCreateChannel $ teamChannelName desiredTeamState
  ensureChannelTopic getDisplayName channel desiredTeamState
  let channelID = channel ^. Channel.id
  traverse_ (ensureGroupState [channelID]) $ groupList desiredTeamState

ensureChannelTopic ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  GetDisplayName ->
  Channel ->
  DesiredTeamState ->
  ExceptT Text (Sem r) ()
ensureChannelTopic getDisplayName channel desiredTeamState = do
  newTopic <- topicGivenDisplayNames desiredTeamState (unGetDisplayName getDisplayName)
  unless (same currentTopic newTopic) $ setChannelTopic channelID newTopic
  where
    channelID = channel ^. Channel.id
    currentTopic = channel ^. Channel.topic
    same oldTopic newTopic = oldTopic == newTopic || clean oldTopic == clean newTopic
    clean = filter (not . potentialAddedChar)
    potentialAddedChar c = c `elem` ['<', '>']

findOrCreateChannel ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  Text ->
  ExceptT Text (Sem r) Channel
findOrCreateChannel name = do
  current <- findChannel name
  maybe (createChannel name) return current

ensureGroupState ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  [Text] ->
  Group ->
  ExceptT Text (Sem r) ()
ensureGroupState defaultChannelIDs group = do
  existingGroup <- findGroup $ handle group
  slackGroup <- maybe createNew return existingGroup
  let groupID = slackGroup ^. Group.id

  currentMembers <- Set.fromList <$> getGroupMembers groupID
  unless (memberIDs group == currentMembers) $ setGroupMembers groupID $ Set.toList $ memberIDs group

  let currentChannels = slackGroup ^. channelIDs
  unless (same defaultChannelIDs currentChannels) $ setGroupChannels groupID defaultChannelIDs
  where
    createNew = createGroup (handle group) (description group) defaultChannelIDs
    same a b = null (a \\ b) && null (b \\ a)

getDisplayNameM ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  ExceptT Text (Sem r) GetDisplayName
getDisplayNameM = do
  map <- Map.fromList . fmap toPair <$> listAllUsers
  return $ GetDisplayName $ lookup map
  where
    lookup map id = Map.lookup id map ?? pack (printf "Could not find user with ID: %s" id)
    toPair user = (user ^. User.id, user ^. displayName)

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
