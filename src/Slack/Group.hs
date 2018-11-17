{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Slack.Group
    ( getGroupMembers
    , setGroupMembers
    , setGroupChannels
    , createGroup
    ) where

import Slack.Util (slackGet, slackPost, Token)
import Data.Text (Text, unpack)
import Network.Wreq (defaults, param)
import Data.Aeson ((.=), Value)
import Control.Lens ((&), (.~), (^?), (^..))
import Data.Aeson.Lens (key, values, _String)
import Data.List (intercalate)
import Control.Monad.Trans.Except (ExceptT)
import Control.Error.Util ((??))

getGroupMembers :: Token -> Text -> ExceptT Text IO [Text]
getGroupMembers apiToken groupID = do
    let opts = defaults & param "usergroup" .~ [groupID]
                        & param "include_disabled" .~ ["true"]
    respBody <- slackGet apiToken opts "usergroups.users.list"
    return $ respBody ^.. key "users" . values . _String

setGroupMembers :: Token -> Text -> [Text] -> ExceptT Text IO ()
setGroupMembers apiToken groupID userIDs = do
    let params = [ "usergroup" .= groupID
                 , "users" .= intercalate "," (unpack <$> userIDs)
                 ]
    _ <- slackPost apiToken params "usergroups.users.update"
    return ()

setGroupChannels :: Token -> Text -> [Text] -> ExceptT Text IO ()
setGroupChannels apiToken groupID defaultChannelIDs = do
    let params = [ "usergroup" .= groupID
                 , "channels" .= intercalate "," (unpack <$> defaultChannelIDs)
                 ]
    _ <- slackPost apiToken params "usergroups.update"
    return ()

createGroup :: Token -> Text -> Text -> [Text] -> ExceptT Text IO Value
createGroup apiToken groupHandle groupName defaultChannelIDs = do
    let params = [ "handle" .= groupHandle
                 , "name" .= groupName
                 , "channels" .= intercalate "," (unpack <$> defaultChannelIDs)
                 ]
    respBody <- slackPost apiToken params "usergroups.create"
    let group = respBody ^? key "usergroup"
    group ?? "\"usergroups.create\" response didn't include a \"usergroup\" key"
