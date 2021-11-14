{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( ensure,
    dryRunEnsure,
    listUsers,
  )
where

import Config
  ( DesiredTeamState (..),
    Group (..),
    Members (..),
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
import Data.List (cycle, elem, nub, null, zip3, (\\))
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, filter, intercalate, pack, unlines)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Traversable (traverse)
import Network.Wreq.Session (newAPISession)
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
import Slack.User as User (displayName, getUser, id, listAllUsers)
import Slack.Util (NetCtx (..))
import Text.Printf (printf)
import Text.Show.Functions ()
import Prelude hiding (filter, unlines)

ensure :: Text -> ByteString -> ExceptT Text IO Text
ensure inputText apiToken = do
  netCtx <- lift (NetCtx apiToken <$> newAPISession)
  records <- lift $ parseTeamList inputText
  getDisplayName <- getDisplayNameM netCtx
  teamResults <- traverse (wrapTeamResult $ ensureTeamState netCtx getDisplayName) records
  return $ unlines teamResults
  where
    wrapTeamResult f record = ("Team " <> team record <> ": success!") <$ f record

getDisplayNameM :: NetCtx -> ExceptT Text IO (Text -> ExceptT Text IO Text)
getDisplayNameM netCtx = do
  map <- Map.fromList . fmap toPair <$> listAllUsers netCtx
  return $ \id -> Map.lookup id map ?? pack (printf "Could not find user with ID: %s" id)
  where
    toPair user = (user ^. User.id, user ^. displayName)

dryRunEnsure :: Text -> ByteString -> ExceptT Text IO Text
dryRunEnsure inputText apiToken = do
  time <- lift getCurrentTime
  desiredStateList <- lift (currentDesiredTeamState time <$$> parseTeamList inputText)
  netCtx <- lift (NetCtx apiToken <$> newAPISession)
  getDisplayName <- getDisplayNameM netCtx
  showDesiredTeamStateList getDisplayName desiredStateList

listUsers :: ByteString -> ExceptT Text IO Text
listUsers apiToken = do
  netCtx <- lift (NetCtx apiToken <$> newAPISession)
  unlines . fmap formatLine <$> listAllUsers netCtx
  where
    formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

ensureTeamState :: NetCtx -> (Text -> ExceptT Text IO Text) -> Team -> ExceptT Text IO ()
ensureTeamState netCtx getDisplayName record = do
  time <- lift getCurrentTime
  let desiredTeamState = currentDesiredTeamState time record
  channel <- findOrCreateChannel netCtx $ teamChannelName desiredTeamState
  ensureChannelTopic netCtx getDisplayName channel desiredTeamState
  let channelID = channel ^. Channel.id
  traverse_ (ensureGroupState netCtx [channelID]) $ groupList desiredTeamState

ensureChannelTopic :: NetCtx -> (Text -> ExceptT Text IO Text) -> Channel -> DesiredTeamState -> ExceptT Text IO ()
ensureChannelTopic netCtx getDisplayName channel desiredTeamState = do
  newTopic <- topicGivenDisplayNames desiredTeamState getDisplayName
  unless (same currentTopic newTopic) $ setChannelTopic netCtx channelID newTopic
  where
    channelID = channel ^. Channel.id
    currentTopic = channel ^. Channel.topic
    same oldTopic newTopic = oldTopic == newTopic || clean oldTopic == clean newTopic
    clean = filter (not . potentialAddedChar)
    potentialAddedChar c = c `elem` ['<', '>']

findOrCreateChannel :: NetCtx -> Text -> ExceptT Text IO Channel
findOrCreateChannel netCtx name = do
  current <- findChannel netCtx name
  maybe (createChannel netCtx name) return current

ensureGroupState :: NetCtx -> [Text] -> Group -> ExceptT Text IO ()
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

(<$$>) f = fmap $ fmap f
