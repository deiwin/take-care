{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.Channel
    ( findChannel
    , createChannel
    , inviteMembers
    , getChannelMembers
    , setChannelTopic
    , Channel
    , id
    , topic
    )
where

import Prelude hiding (id)
import Slack.Util (slackGetPaginated, slackPost, NetCtx, fromJSON)
import Data.Text (Text, unpack)
import Data.Traversable (traverse)
import Network.Wreq (defaults, param)
import Control.Lens ((&), (.~), (^?), (^.), (^..))
import Control.Lens.TH (makeLenses)
import Data.Aeson ((.=), FromJSON(parseJSON), withObject, (.:))
import Data.Aeson.Lens (key, values, _String)
import GHC.Generics (Generic)
import Data.List as L (find, intercalate)
import Control.Monad.Trans.Except (ExceptT)
import Control.Error.Util ((??))

data Channel = Channel { _id :: Text
                       , _name :: Text
                       , _topic :: Text
                       } deriving (Generic, Show)
$(makeLenses ''Channel)

instance FromJSON Channel where
    parseJSON = withObject "Channel" $ \o -> do
        _id <- o .: "id"
        _name <- o .: "name"
        topicObject <- o .: "topic"
        _topic <- topicObject .: "value"
        return Channel{..}

findChannel :: NetCtx -> Text -> ExceptT Text IO (Maybe Channel)
findChannel netCtx expectedName = do
    let params = defaults & param "types" .~ [ "public_channel,private_channel" ]
                          & param "exclude_archived" .~ [ "true" ]
                          & param "limit" .~ [ "1000" ]
    respBodies <- slackGetPaginated netCtx params "conversations.list"
    channels   <-
        traverse fromJSON
        .   concatMap (^.. values)
        =<< (traverse (^? key "channels") respBodies ?? "\"users.list\" response didn't include a \"channels\" field")
    return $ find (\x -> (x ^. name) == expectedName) channels

createChannel :: NetCtx -> Text -> [Text] -> ExceptT Text IO Channel
createChannel netCtx newName userIDs = do
    let params = [ "name" .= newName
                 , "user_ids" .= intercalate "," (unpack <$> userIDs)
                 ]
    respBody <- slackPost netCtx params "conversations.create"
    val      <- (respBody ^? key "channel") ?? "\"conversations.create\" response didn't include a \"channel\" key"
    fromJSON val

inviteMembers :: NetCtx -> Text -> [Text] -> ExceptT Text IO ()
inviteMembers netCtx channelID userIDs = do
    let params = [ "channel" .= channelID
                 , "users" .= intercalate "," (unpack <$> userIDs)
                 ]
    _ <- slackPost netCtx params "conversations.invite"
    return ()

getChannelMembers :: NetCtx -> Text -> ExceptT Text IO [Text]
getChannelMembers netCtx channelID = do
    let opts = defaults & param "channel" .~ [channelID]
    respBodies <- slackGetPaginated netCtx opts "conversations.members"
    return $ respBodies ^.. traverse . key "members" . values . _String

setChannelTopic :: NetCtx -> Text -> Text -> ExceptT Text IO ()
setChannelTopic netCtx channelID newTopic = do
    let params = [ "channel" .= channelID
                 , "topic" .= newTopic
                 ]
    _ <- slackPost netCtx params "conversations.setTopic"
    return ()
