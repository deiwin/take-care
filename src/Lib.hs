{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Lib
    ( ensure
    , listCaretakers
    , listUsers
    )
where

import Prelude hiding (unlines, filter)
import Slack.Util (Token)
import Slack.Channel as Channel (findChannel, createChannel, inviteMembers, getChannelMembers
  , setChannelTopic, Channel, id, topic)
import Slack.Group as Group (getGroupMembers, setGroupMembers, setGroupChannels, findGroup
  , createGroup, id, channelIDs)
import Slack.User as User (getUser, listAllUsers, id, displayName)
import Dhall (input, auto, Interpret)
import Data.Text (Text, unlines, pack, filter)
import Text.Printf (printf)
import GHC.Generics (Generic)
import Text.Show.Functions ()
import Data.Traversable (traverse)
import Control.Monad (unless)
import Control.Lens ((^.))
import Data.List (null, (\\), cycle, zip3, elem)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import Data.Time.Calendar.WeekDate (toWeekDate)

data Team = Team { members :: [Text]
                 , team :: Text -- TODO should be max 21 chars with the tm- prefix, so 18
                 , topic :: Text -> Text
                 } deriving (Generic, Show)

instance Interpret Team

ensure :: Text -> Token -> ExceptT Text IO Text
ensure inputText apiToken = do
    records      <- lift $ input auto inputText
    teamResults  <- traverse (wrapTeamResult $ ensureTeamState apiToken) records
    caretakerIDs <- traverse (lift . getCaretaker) (members <$> records)
    groupResult  <- wrapGroupResult <$> ensureGroupState apiToken groupHandle groupName groupChannels caretakerIDs
    return $ unlines (teamResults ++ [groupResult])
  where
    wrapTeamResult f record = const ("Team " <> team record <> ": success!") <$> f record
    wrapGroupResult = const "Caretakers group: success!"
    groupHandle     = "caretakers"
    groupName       = "Current caretakers of every team"
    groupChannels   = []

listCaretakers :: Text -> Token -> ExceptT Text IO Text
listCaretakers inputText apiToken = do
    records               <- lift $ input auto inputText
    caretakerIDs          <- traverse (lift . getCaretaker) (members <$> records)
    caretakerDisplayNames <- traverse (fmap (^. displayName) . getUser apiToken) caretakerIDs
    return $ unlines $ formatLine <$> zip3 (team <$> records) caretakerDisplayNames caretakerIDs
    where formatLine (teamName, userName, userID) = pack $ printf "Team %s: %s (%s)" teamName userName userID

listUsers :: Token -> ExceptT Text IO Text
listUsers apiToken = unlines . fmap formatLine <$> listAllUsers apiToken
    where formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

ensureTeamState :: Token -> Team -> ExceptT Text IO ()
ensureTeamState apiToken record = do
    channel <- findOrCreateChannel apiToken channelName userIDs
    let channelID = channel ^. Channel.id
    ensureAllMembersPresent apiToken channelID userIDs
    caretakerID <- lift $ getCaretaker userIDs
    ensureChannelTopic apiToken channel (Lib.topic record) caretakerID
    ensureGroupState apiToken teamGroupHandle      teamGroupName      [channelID] userIDs
    ensureGroupState apiToken caretakerGroupHandle caretakerGroupName [channelID] [caretakerID]
  where
    channelName          = "tm-" <> team record
    teamGroupHandle      = team record
    teamGroupName        = "Team " <> teamGroupHandle
    caretakerGroupHandle = teamGroupHandle <> "-caretaker"
    caretakerGroupName   = teamGroupName <> " caretaker"
    userIDs              = members record

ensureChannelTopic :: Token -> Channel -> (Text -> Text) -> Text -> ExceptT Text IO ()
ensureChannelTopic apiToken channel buildTopic caretakerID = do
    caretakerDisplayName <- (^. displayName) <$> getUser apiToken caretakerID
    let newTopic = buildTopic caretakerDisplayName
    unless (same currentTopic newTopic) $ setChannelTopic apiToken channelID newTopic
  where
    channelID    = channel ^. Channel.id
    currentTopic = channel ^. Channel.topic
    same oldTopic newTopic = oldTopic == newTopic || clean oldTopic == clean newTopic
    clean = filter (not . potentialAddedChar)
    potentialAddedChar c = c `elem` ['<', '>']

getCaretaker :: [Text] -> IO Text
getCaretaker userIDs = do
    (_, currentUtcWeek, _) <- toWeekDate . utctDay <$> getCurrentTime
    return $ cycle userIDs !! currentUtcWeek

findOrCreateChannel :: Token -> Text -> [Text] -> ExceptT Text IO Channel
findOrCreateChannel apiToken name userIDs = do
    current <- findChannel apiToken name
    maybe (createChannel apiToken name userIDs) return current

ensureGroupState :: Token -> Text -> Text -> [Text] -> [Text] -> ExceptT Text IO ()
ensureGroupState apiToken groupHandle groupName defaultChannelIDs userIDs = do
    existingGroup <- findGroup apiToken groupHandle
    group         <- maybe createNew return existingGroup
    let groupID = group ^. Group.id

    currentMembers <- getGroupMembers apiToken groupID
    unless (same userIDs currentMembers) $ setGroupMembers apiToken groupID userIDs

    let currentChannels = group ^. channelIDs
    unless (same defaultChannelIDs currentChannels) $ setGroupChannels apiToken groupID defaultChannelIDs
  where
    createNew = createGroup apiToken groupHandle groupName defaultChannelIDs
    same a b = null (a \\ b) && null (b \\ a)

ensureAllMembersPresent :: Token -> Text -> [Text] -> ExceptT Text IO ()
ensureAllMembersPresent apiToken channelID userIDs = do
    currentMembers <- getChannelMembers apiToken channelID
    let missingMembers = userIDs \\ currentMembers
    unless (null missingMembers) $ inviteMembers apiToken channelID missingMembers
