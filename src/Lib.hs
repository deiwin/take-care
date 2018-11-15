{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}

module Lib
    ( someFunc
    ) where

import Dhall (input, auto, Interpret)
import Data.Text as T (Text, pack, unpack, null)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Text.Show.Functions ()
import System.FilePath (FilePath)
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Control.Monad ((>=>), mfilter, unless)
import Network.Wreq (defaults, param, header, getWith, postWith, asValue, responseBody, auth
  , oauth2Bearer, Options, Response)
import Data.Maybe (maybeToList, fromMaybe)
import Data.Aeson (FromJSON, encode, toJSON, pairs, (.=), object, Value)
import Data.Aeson.Types (Pair)
import Control.Lens ((&), (.~), (^?), (^?!), (^..), (^.), (?~), _Just)
import Data.Aeson.Lens (key, values, _String, _Bool)
import Data.List as L (find, intercalate, null, (\\), cycle)
import Data.Map as Map (fromList)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Error.Util (hoistEither, (??))
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import Data.Time.Calendar.WeekDate (toWeekDate)

import Debug.Trace (trace)

type Token = ByteString
data InputRecord = InputRecord { members :: [Text]
                               , team :: Text -- TODO should be max 21 chars with the tm- prefix, so 18
                               , topic :: Text -> Text
                               } deriving (Generic, Show)

instance Interpret InputRecord

readInput :: FilePath -> IO [InputRecord]
readInput = input auto . T.pack

someFunc :: FilePath -> Token -> IO ()
someFunc inputPath apiToken = do
    input <- readInput inputPath
    traverse_ print input
    results <- traverse (runExceptT . ensureTeamState apiToken) input
    print results

ensureTeamState :: Token -> InputRecord -> ExceptT Text IO ()
ensureTeamState apiToken record = do
    channel <- findOrCreateChannel apiToken channelName userIDs
    let channelID = channel ^?! key "id" . _String
    lift $ print $ "cid: " <> channelID
    ensureAllMembersPresent apiToken channelID userIDs
    caretakerID <- lift $ getCaretaker userIDs
    setChannelTopic apiToken channelID (topic record) caretakerID
    ensureGroupState apiToken teamGroupName [channelID] userIDs
    ensureGroupState apiToken caretakerGroupName [channelID] [caretakerID]
  where
    channelName = "tm-" <> team record
    teamGroupName = team record
    caretakerGroupName = teamGroupName <> "-caretaker"
    userIDs = members record

setChannelTopic :: Token -> Text -> (Text -> Text) -> Text -> ExceptT Text IO ()
setChannelTopic apiToken channelID buildTopic caretakerID = do
    caretakerDisplayName <- getDisplayName apiToken caretakerID
    let params = [ "channel" .= channelID
                 , "topic" .= buildTopic caretakerDisplayName
                 ]
    slackPost apiToken params "conversations.setTopic"
    return ()

getDisplayName :: Token -> Text -> ExceptT Text IO Text
getDisplayName apiToken id = do
    let opts = defaults & param "user" .~ [id]
    respBody <- slackGet apiToken opts "users.info"
    let displayName = ("@" <>) <$> respBody ^? key "user" . key "profile" . key "display_name" . _String
    displayName ?? "\"users.info\" response didn't include \"user.profile.display_name\" field"

getCaretaker :: [Text] -> IO Text
getCaretaker userIDs = do
    (_, currentUtcWeek, _) <- (toWeekDate . utctDay) <$> getCurrentTime
    return $ cycle userIDs !! currentUtcWeek

findOrCreateChannel :: Token -> Text -> [Text] -> ExceptT Text IO Value
findOrCreateChannel apiToken name userIDs = do
    current <- findChannel name apiToken
    maybe (createChannel apiToken name userIDs) return current

createChannel :: Token -> Text -> [Text] -> ExceptT Text IO Value
createChannel apiToken name userIDs = do
    let params = [ "name" .= name
                 , "user_ids" .= intercalate "," (T.unpack <$> userIDs)
                 ]
    respBody <- slackPost apiToken params "conversations.create"
    (respBody ^? key "channel") ?? "\"conversations.create\" response didn't include a \"channel\" key"

slackURL :: String -> String
slackURL = ("https://slack.com/api/" ++)

slackGet :: Token -> Options -> String -> ExceptT Text IO Value
slackGet apiToken opts method = do
    let optsWithAuth = opts & auth ?~ oauth2Bearer apiToken
    let url = slackURL method
    resp <- lift (asValue =<< getWith optsWithAuth url)
    hoistEither $ handleSlackError "GET" method resp

slackGetPaginated :: Token -> Options -> String -> ExceptT Text IO [Value]
slackGetPaginated = slackGetPaginated' Nothing []
slackGetPaginated' :: Maybe Text -> [Value] -> Token -> Options -> String -> ExceptT Text IO [Value]
slackGetPaginated' cursor !acc apiToken opts method = do
    let optsWithCursor = opts & param "cursor" .~ maybeToList cursor
    respBody <- slackGet apiToken optsWithCursor method
    let
      nextCursor = mfilter (not . T.null) $
          respBody ^? key "response_metadata" . key "next_cursor" . _String
    let nextAcc = respBody : acc
    case nextCursor of
      Just c -> slackGetPaginated' nextCursor nextAcc apiToken opts method
      Nothing -> return $ reverse nextAcc

slackPost :: Token -> [Pair] -> String -> ExceptT Text IO Value
slackPost apiToken params method = do
    let optsWithAuth = defaults & auth ?~ oauth2Bearer apiToken
    let url = slackURL method
    let body = toJSON $ object params
    resp <- lift $ asValue =<< postWith optsWithAuth url body
    hoistEither $ handleSlackError "POST" method resp

handleSlackError :: Text -> String -> Response Value -> Either Text Value
handleSlackError httpMethod method resp =
    let respBody = resp ^. responseBody
        ok = respBody ^?! key "ok" . _Bool
        error = respBody ^. key "error" . _String
        detail = respBody ^? key "detail" . _String
     in if ok then
            Right respBody
        else
            Left (httpMethod <> " " <> T.pack method <> ": " <> error <> maybe "" (" - " <>) detail)

ensureGroupState :: Token -> Text -> [Text] -> [Text] -> ExceptT Text IO ()
ensureGroupState apiToken groupName defaultChannelIDs userIDs = do
    let opts = defaults & param "include_disabled" .~ ["true"]
    respBody <- slackGet apiToken opts "usergroups.list"
    let
        existingGroup = find (\x -> (x ^? key "handle". _String) == Just groupName) $
            respBody ^.. key "usergroups" . values
    group <- maybe createNew return existingGroup
    let groupID = getID group

    currentMembers <- getGroupMembers apiToken groupID
    unless (same userIDs currentMembers) $ setGroupMembers apiToken groupID userIDs

    let currentChannels = group ^.. key "prefs" . key "channels" . values . _String
    unless (same defaultChannelIDs currentChannels) $ setGroupChannels apiToken groupID defaultChannelIDs
  where
    createNew = createGroup apiToken groupName defaultChannelIDs
    getID group = group ^. key "id" . _String
    same a b = L.null (a \\ b) && L.null (b \\ a)

getGroupMembers :: Token -> Text -> ExceptT Text IO [Text]
getGroupMembers apiToken groupID = do
    let opts = defaults & param "usergroup" .~ [groupID]
                        & param "include_disabled" .~ ["true"]
    respBody <- slackGet apiToken opts "usergroups.users.list"
    return $ respBody ^.. key "users" . values . _String

setGroupMembers :: Token -> Text -> [Text] -> ExceptT Text IO ()
setGroupMembers apiToken groupID userIDs = do
    let params = [ "usergroup" .= groupID
                 , "users" .= intercalate "," (T.unpack <$> userIDs)
                 ]
    slackPost apiToken params "usergroups.users.update"
    return ()

setGroupChannels :: Token -> Text -> [Text] -> ExceptT Text IO ()
setGroupChannels apiToken groupID defaultChannelIDs = do
    let params = [ "usergroup" .= groupID
                 , "channels" .= intercalate "," (T.unpack <$> defaultChannelIDs)
                 ]
    slackPost apiToken params "usergroups.update"
    return ()

createGroup :: Token -> Text -> [Text] -> ExceptT Text IO Value
createGroup apiToken groupName defaultChannelIDs = do
    let params = [ "handle" .= groupName
                 , "name" .= ("Team " <> groupName)
                 , "channels" .= intercalate "," (T.unpack <$> defaultChannelIDs)
                 ]
    respBody <- slackPost apiToken params "usergroups.create"
    let group = respBody ^? key "usergroup"
    group ?? "\"usergroups.create\" response didn't include a \"usergroup\" key"

ensureAllMembersPresent :: Token -> Text -> [Text] -> ExceptT Text IO ()
ensureAllMembersPresent apiToken channelID userIDs = do
    currentMembers <- getChannelMembers apiToken channelID
    let missingMembers = userIDs \\ currentMembers
    if L.null missingMembers
       then return ()
       else inviteMembers apiToken channelID missingMembers

getChannelMembers :: Token -> Text -> ExceptT Text IO [Text]
getChannelMembers apiToken channelID = do
    let opts = defaults & param "channel" .~ [channelID]
    respBodies <- slackGetPaginated apiToken opts "conversations.members"
    return $ respBodies ^.. traverse . key "members" . values . _String

inviteMembers :: Token -> Text -> [Text] -> ExceptT Text IO ()
inviteMembers apiToken channelID userIDs = do
    let params = [ "channel" .= channelID
                 , "users" .= intercalate "," (T.unpack <$> userIDs)
                 ]
    slackPost apiToken params "conversations.invite"
    return ()

findChannel :: Text -> Token -> ExceptT Text IO (Maybe Value)
findChannel name apiToken = do
    respBodies <- slackGetPaginated apiToken defaults "conversations.list"
    return $ find (\x -> (x ^? key "name". _String) == Just name) $
        respBodies ^.. traverse . key "channels" . values
