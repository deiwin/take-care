{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Lib
    ( ensure
    , listCaretakers
    , listUsers
    ) where

import Prelude hiding (id)
import Slack.Util (slackGet, Token)
import Slack.Channel (findChannel, createChannel, inviteMembers, getChannelMembers, setChannelTopic)
import Slack.Group (getGroupMembers, setGroupMembers, setGroupChannels, createGroup)
import Slack.User (getUser, listAllUsers, id, displayName)
import Dhall (input, auto, Interpret)
import Data.Text (Text)
import Text.Printf (printf)
import GHC.Generics (Generic)
import Text.Show.Functions ()
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Control.Monad (unless, void)
import Network.Wreq (defaults, param)
import Data.Aeson (Value)
import Control.Lens ((&), (.~), (^?), (^?!), (^..), (^.))
import Data.Aeson.Lens (key, values, _String)
import Data.List (find, null, (\\), cycle, zip3)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import Data.Time.Calendar.WeekDate (toWeekDate)

data InputRecord = InputRecord { members :: [Text]
                               , team :: Text -- TODO should be max 21 chars with the tm- prefix, so 18
                               , topic :: Text -> Text
                               } deriving (Generic, Show)

instance Interpret InputRecord

ensure :: Text -> Token -> IO ()
ensure inputText apiToken = do
    records <- input auto inputText
    traverse_ print records
    ensureStateOfAllTeams apiToken records
    return ()

listCaretakers :: Text -> Token -> IO ()
listCaretakers inputText apiToken = (void . runExceptT) $ do
    records <- lift $ input auto inputText
    caretakerIDs <- traverse (lift . getCaretaker) (members <$> records)
    caretakerDisplayNames <- traverse (fmap (^. displayName) . getUser apiToken) caretakerIDs
    traverse_ (lift . printLine) $ zip3 (team <$> records) caretakerDisplayNames caretakerIDs
  where
    printLine (teamName, userName, userID) = printf "Team %s: %s (%s)\n" teamName userName userID

listUsers :: Token -> IO ()
listUsers apiToken = (void . runExceptT) $
    traverse_ (lift . printLine) =<< listAllUsers apiToken
  where
    printLine user = printf "%s: %s\n" (user ^. id) (user ^. displayName)

ensureStateOfAllTeams :: Token -> [InputRecord] -> IO ()
ensureStateOfAllTeams apiToken records = do
    caretakerIDs <- traverse getCaretaker (members <$> records)
    results <- traverse (runExceptT . ensureTeamState apiToken) records
    result <- runExceptT $ ensureGroupState apiToken groupHandle groupName groupChannels caretakerIDs
    print (results ++ [result])
  where
    groupHandle = "caretakers"
    groupName = "Current caretakers of every team"
    groupChannels = []

ensureTeamState :: Token -> InputRecord -> ExceptT Text IO ()
ensureTeamState apiToken record = do
    channel <- findOrCreateChannel apiToken channelName userIDs
    let channelID = channel ^?! key "id" . _String
    lift $ print $ "cid: " <> channelID
    ensureAllMembersPresent apiToken channelID userIDs
    caretakerID <- lift $ getCaretaker userIDs
    ensureChannelTopic apiToken channel (topic record) caretakerID
    ensureGroupState apiToken teamGroupHandle teamGroupName [channelID] userIDs
    ensureGroupState apiToken caretakerGroupHandle caretakerGroupName [channelID] [caretakerID]
  where
    channelName = "tm-" <> team record
    teamGroupHandle = team record
    teamGroupName = "Team " <> teamGroupHandle
    caretakerGroupHandle = teamGroupHandle <> "-caretaker"
    caretakerGroupName = teamGroupName <> " caretaker"
    userIDs = members record

ensureChannelTopic :: Token -> Value -> (Text -> Text) -> Text -> ExceptT Text IO ()
ensureChannelTopic apiToken channel buildTopic caretakerID = do
    caretakerDisplayName <- (^. displayName) <$> getUser apiToken caretakerID
    let newTopic = buildTopic caretakerDisplayName
    unless (currentTopic == newTopic) $ setChannelTopic apiToken channelID newTopic
  where
    channelID = channel ^?! key "id" . _String
    currentTopic = channel ^?! key "topic" . key "value" . _String

getCaretaker :: [Text] -> IO Text
getCaretaker userIDs = do
    (_, currentUtcWeek, _) <- (toWeekDate . utctDay) <$> getCurrentTime
    return $ cycle userIDs !! currentUtcWeek

findOrCreateChannel :: Token -> Text -> [Text] -> ExceptT Text IO Value
findOrCreateChannel apiToken name userIDs = do
    current <- findChannel name apiToken
    maybe (createChannel apiToken name userIDs) return current

ensureGroupState :: Token -> Text -> Text -> [Text] -> [Text] -> ExceptT Text IO ()
ensureGroupState apiToken groupHandle groupName defaultChannelIDs userIDs = do
    let opts = defaults & param "include_disabled" .~ ["true"]
    respBody <- slackGet apiToken opts "usergroups.list"
    let
        existingGroup = find (\x -> (x ^? key "handle". _String) == Just groupHandle) $
            respBody ^.. key "usergroups" . values
    group <- maybe createNew return existingGroup
    let groupID = getID group

    currentMembers <- getGroupMembers apiToken groupID
    unless (same userIDs currentMembers) $ setGroupMembers apiToken groupID userIDs

    let currentChannels = group ^.. key "prefs" . key "channels" . values . _String
    unless (same defaultChannelIDs currentChannels) $ setGroupChannels apiToken groupID defaultChannelIDs
  where
    createNew = createGroup apiToken groupHandle groupName defaultChannelIDs
    getID group = group ^. key "id" . _String
    same a b = null (a \\ b) && null (b \\ a)

ensureAllMembersPresent :: Token -> Text -> [Text] -> ExceptT Text IO ()
ensureAllMembersPresent apiToken channelID userIDs = do
    currentMembers <- getChannelMembers apiToken channelID
    let missingMembers = userIDs \\ currentMembers
    unless (null missingMembers) $ inviteMembers apiToken channelID missingMembers
