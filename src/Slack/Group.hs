{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Slack.Group
  ( getGroupMembers,
    setGroupMembers,
    setGroupChannels,
    findGroup,
    createGroup,
    Group,
    id,
    handle,
    channelIDs,
  )
where

import Control.Error.Util ((??))
import Control.Lens ((&), (.~), (^.), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.=))
import Data.Aeson.Lens (key, values, _String)
import Data.List (find, intercalate)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param)
import Slack.Util (NetCtx, fromJSON, slackGet, slackPost)
import Prelude hiding (id)

data Group = Group
  { _id :: Text,
    _handle :: Text,
    _channelIDs :: [Text]
  }
  deriving (Generic, Show)

$(makeLenses ''Group)

instance FromJSON Group where
  parseJSON = withObject "Group" $ \o -> do
    _id <- o .: "id"
    _handle <- o .: "handle"
    prefs <- o .: "prefs"
    _channelIDs <- prefs .: "channels"
    return Group {..}

getGroupMembers :: NetCtx -> Text -> ExceptT Text IO [Text]
getGroupMembers netCtx groupID = do
  let opts =
        defaults & param "usergroup" .~ [groupID]
          & param "include_disabled" .~ ["true"]
  respBody <- slackGet netCtx opts "usergroups.users.list"
  return $ respBody ^.. key "users" . values . _String

setGroupMembers :: NetCtx -> Text -> [Text] -> ExceptT Text IO ()
setGroupMembers netCtx groupID userIDs = do
  let params =
        [ "usergroup" .= groupID,
          "users" .= intercalate "," (unpack <$> userIDs)
        ]
  _ <- slackPost netCtx params "usergroups.users.update"
  return ()

setGroupChannels :: NetCtx -> Text -> [Text] -> ExceptT Text IO ()
setGroupChannels netCtx groupID defaultChannelIDs = do
  let params =
        [ "usergroup" .= groupID,
          "channels" .= intercalate "," (unpack <$> defaultChannelIDs)
        ]
  _ <- slackPost netCtx params "usergroups.update"
  return ()

findGroup :: NetCtx -> Text -> ExceptT Text IO (Maybe Group)
findGroup netCtx expectedHandle = do
  let opts = defaults & param "include_disabled" .~ ["true"]
  respBody <- slackGet netCtx opts "usergroups.list"
  groups <-
    traverse fromJSON
      . (^.. values)
      =<< ((respBody ^? key "usergroups") ?? "\"users.list\" response didn't include a \"channels\" field")
  return $ find (\x -> (x ^. handle) == expectedHandle) groups

createGroup :: NetCtx -> Text -> Text -> [Text] -> ExceptT Text IO Group
createGroup netCtx groupHandle groupName defaultChannelIDs = do
  let params =
        [ "handle" .= groupHandle,
          "name" .= groupName,
          "channels" .= intercalate "," (unpack <$> defaultChannelIDs)
        ]
  respBody <- slackPost netCtx params "usergroups.create"
  val <- (respBody ^? key "usergroup") ?? "\"usergroups.create\" response didn't include a \"usergroup\" key"
  fromJSON val
