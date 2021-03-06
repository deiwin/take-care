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
import Slack.Util (NetCtx(..))
import Slack.Channel as Channel (findChannel, createChannel, inviteMembers, getChannelMembers
  , setChannelTopic, Channel, id, topic)
import Slack.Group as Group (getGroupMembers, setGroupMembers, setGroupChannels, findGroup
  , createGroup, id, channelIDs)
import Slack.User as User (getUser, listAllUsers, id, displayName)
import Dhall (input, auto, FromDhall)
import Data.Text (Text, unlines, pack, filter, intercalate)
import Data.ByteString (ByteString)
import Text.Printf (printf)
import GHC.Generics (Generic)
import Text.Show.Functions ()
import Data.Traversable (traverse)
import Control.Monad (unless)
import Control.Lens ((^.))
import Data.List (null, (\\), cycle, zip3, elem, nub)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import Data.Time.Calendar.WeekDate (toWeekDate)
import Network.Wreq.Session (newAPISession)

data Members = Members { caretakers :: [[Text]]
                       , others :: [Text]
                       } deriving (Generic, Show)
instance FromDhall Members

data Team = Team { members :: Members
                 , team :: Text -- TODO should be max 21 chars with the tm- prefix, so 18
                 , topic :: Text -> Text
                 } deriving (Generic, Show)
instance FromDhall Team

ensure :: Text -> ByteString -> ExceptT Text IO Text
ensure inputText apiToken = do
    sess         <- lift newAPISession
    let netCtx    = NetCtx apiToken sess
    records      <- lift $ input auto inputText
    teamResults  <- traverse (wrapTeamResult $ ensureTeamState netCtx) records
    caretakerIDs <- fmap nub $ traverse (lift . getCaretaker) $ concat (caretakers . members <$> records)
    groupResult  <- wrapGroupResult <$> ensureGroupState netCtx groupHandle groupName groupChannels caretakerIDs
    return $ unlines (teamResults ++ [groupResult])
  where
    wrapTeamResult f record = ("Team " <> team record <> ": success!") <$ f record
    wrapGroupResult = const "Caretakers group: success!"
    groupHandle     = "caretakers"
    groupName       = "Current caretakers of every team"
    groupChannels   = []

listCaretakers :: Text -> ByteString -> ExceptT Text IO Text
listCaretakers inputText apiToken = do
    sess                  <- lift newAPISession
    let netCtx             = NetCtx apiToken sess
    records :: [Team]     <- lift $ input auto inputText
    let teams              = concat ((\r -> team r <$ (caretakers . members) r) <$> records)
    caretakerIDs          <- traverse (lift . getCaretaker) $ concat (caretakers . members <$> records)
    caretakerDisplayNames <- traverse (fmap (^. displayName) . getUser netCtx) caretakerIDs
    return $ unlines $ formatLine <$> zip3 teams caretakerDisplayNames caretakerIDs
    where formatLine (teamName, userName, userID) = pack $ printf "Team %s: %s (%s)" teamName userName userID

listUsers :: ByteString -> ExceptT Text IO Text
listUsers apiToken = do
    sess                  <- lift newAPISession
    let netCtx             = NetCtx apiToken sess
    unlines . fmap formatLine <$> listAllUsers netCtx
    where formatLine user = pack $ printf "%s: %s" (user ^. User.id) (user ^. displayName)

ensureTeamState :: NetCtx -> Team -> ExceptT Text IO ()
ensureTeamState netCtx record = do
    channel <- findOrCreateChannel netCtx channelName allUserIDs
    let channelID = channel ^. Channel.id
    ensureAllMembersPresent netCtx channelID allUserIDs
    caretakerIDs <- fmap nub $ traverse (lift . getCaretaker) $ caretakers $ members record
    ensureChannelTopic netCtx channel (Lib.topic record) caretakerIDs
    ensureGroupState netCtx teamGroupHandle      teamGroupName      [channelID] allUserIDs
    ensureGroupState netCtx caretakerGroupHandle caretakerGroupName [channelID] caretakerIDs
  where
    channelName          = "tm-" <> team record
    teamGroupHandle      = team record <> "-team"
    teamGroupName        = "Team " <> team record
    caretakerGroupHandle = team record <> "-caretaker"
    caretakerGroupName   = teamGroupName <> " caretaker"
    allCaretakerIDs      = concat $ caretakers $ members record
    othersIDs            = others $ members record
    allUserIDs           = nub (allCaretakerIDs ++ othersIDs)

ensureChannelTopic :: NetCtx -> Channel -> (Text -> Text) -> [Text] -> ExceptT Text IO ()
ensureChannelTopic netCtx channel buildTopic caretakerIDs = do
    caretakerDisplayNames <- traverse (fmap (^. displayName) . getUser netCtx) caretakerIDs
    let newTopic = buildTopic $ intercalate ", " caretakerDisplayNames
    unless (same currentTopic newTopic) $ setChannelTopic netCtx channelID newTopic
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

findOrCreateChannel :: NetCtx -> Text -> [Text] -> ExceptT Text IO Channel
findOrCreateChannel netCtx name userIDs = do
    current <- findChannel netCtx name
    maybe (createChannel netCtx name userIDs) return current

ensureGroupState :: NetCtx -> Text -> Text -> [Text] -> [Text] -> ExceptT Text IO ()
ensureGroupState netCtx groupHandle groupName defaultChannelIDs userIDs = do
    existingGroup <- findGroup netCtx groupHandle
    group         <- maybe createNew return existingGroup
    let groupID = group ^. Group.id

    currentMembers <- getGroupMembers netCtx groupID
    unless (same userIDs currentMembers) $ setGroupMembers netCtx groupID userIDs

    let currentChannels = group ^. channelIDs
    unless (same defaultChannelIDs currentChannels) $ setGroupChannels netCtx groupID defaultChannelIDs
  where
    createNew = createGroup netCtx groupHandle groupName defaultChannelIDs
    same a b = null (a \\ b) && null (b \\ a)

ensureAllMembersPresent :: NetCtx -> Text -> [Text] -> ExceptT Text IO ()
ensureAllMembersPresent netCtx channelID userIDs = do
    currentMembers <- getChannelMembers netCtx channelID
    let missingMembers = userIDs \\ currentMembers
    unless (null missingMembers) $ inviteMembers netCtx channelID missingMembers
