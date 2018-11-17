{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Slack.Channel
    ( findChannel
    , createChannel
    , inviteMembers
    , getChannelMembers
    , setChannelTopic
    ) where

import Slack.Util (slackGetPaginated, slackPost, Token)
import Data.Text (Text, unpack)
import Data.Traversable (traverse)
import Network.Wreq (defaults, param)
import Data.Aeson ((.=), Value)
import Control.Lens ((&), (.~), (^?), (^..))
import Data.Aeson.Lens (key, values, _String)
import Data.List as L (find, intercalate)
import Control.Monad.Trans.Except (ExceptT)
import Control.Error.Util ((??))

findChannel :: Text -> Token -> ExceptT Text IO (Maybe Value)
findChannel name apiToken = do
    respBodies <- slackGetPaginated apiToken defaults "conversations.list"
    return $ find (\x -> (x ^? key "name". _String) == Just name) $
        respBodies ^.. traverse . key "channels" . values

createChannel :: Token -> Text -> [Text] -> ExceptT Text IO Value
createChannel apiToken name userIDs = do
    let params = [ "name" .= name
                 , "user_ids" .= intercalate "," (unpack <$> userIDs)
                 ]
    respBody <- slackPost apiToken params "conversations.create"
    (respBody ^? key "channel") ?? "\"conversations.create\" response didn't include a \"channel\" key"

inviteMembers :: Token -> Text -> [Text] -> ExceptT Text IO ()
inviteMembers apiToken channelID userIDs = do
    let params = [ "channel" .= channelID
                 , "users" .= intercalate "," (unpack <$> userIDs)
                 ]
    _ <- slackPost apiToken params "conversations.invite"
    return ()

getChannelMembers :: Token -> Text -> ExceptT Text IO [Text]
getChannelMembers apiToken channelID = do
    let opts = defaults & param "channel" .~ [channelID]
    respBodies <- slackGetPaginated apiToken opts "conversations.members"
    return $ respBodies ^.. traverse . key "members" . values . _String

setChannelTopic :: Token -> Text -> Text -> ExceptT Text IO ()
setChannelTopic apiToken channelID newTopic = do
    let params = [ "channel" .= channelID
                 , "topic" .= newTopic
                 ]
    _ <- slackPost apiToken params "conversations.setTopic"
    return ()
