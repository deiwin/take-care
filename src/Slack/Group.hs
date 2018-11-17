{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.Group
    ( getGroupMembers
    , setGroupMembers
    , setGroupChannels
    , findGroup
    , createGroup
    , Group
    , id
    , handle
    , channelIDs
    ) where

import Prelude hiding (id)
import Slack.Util (slackGet, slackPost, Token, fromJSON)
import Data.Text (Text, unpack)
import Network.Wreq (defaults, param)
import Control.Lens ((&), (.~), (^?), (^.), (^..))
import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import Data.Aeson ((.=), FromJSON(parseJSON), withObject, (.:))
import Data.Aeson.Lens (key, values, _String)
import Data.List (find, intercalate)
import Control.Monad.Trans.Except (ExceptT)
import Control.Error.Util ((??))

data Group = Group { _id :: Text
                   , _handle :: Text
                   , _channelIDs :: [Text]
                   } deriving (Generic, Show)
$(makeLenses ''Group)

instance FromJSON Group where
    parseJSON = withObject "Group" $ \o -> do
        _id <- o .: "id"
        _handle <- o .: "handle"
        prefs <- o .: "prefs"
        _channelIDs <- prefs .: "channels"
        return Group{..}

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

findGroup :: Token -> Text -> ExceptT Text IO (Maybe Group)
findGroup apiToken expectedHandle = do
    let opts = defaults & param "include_disabled" .~ ["true"]
    respBody <- slackGet apiToken opts "usergroups.list"
    groups <- traverse fromJSON =<< (^.. values) <$>
        ((respBody ^? key "usergroups") ??
            "\"users.list\" response didn't include a \"channels\" field")
    return $ find (\x -> (x ^. handle) == expectedHandle) groups

createGroup :: Token -> Text -> Text -> [Text] -> ExceptT Text IO Group
createGroup apiToken groupHandle groupName defaultChannelIDs = do
    let params = [ "handle" .= groupHandle
                 , "name" .= groupName
                 , "channels" .= intercalate "," (unpack <$> defaultChannelIDs)
                 ]
    respBody <- slackPost apiToken params "usergroups.create"
    val <- (respBody ^? key "usergroup") ?? "\"usergroups.create\" response didn't include a \"usergroup\" key"
    fromJSON val
