{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Slack.Channel
  ( findChannel,
    createChannel,
    setChannelTopic,
    Channel,
    id,
    topic,
  )
where

import Control.Error.Util ((??))
import Control.Lens ((&), (.~), (^.), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.=))
import Data.Aeson.Lens (key, values, _String)
import Data.List as L (find, intercalate)
import Data.Text (Text, unpack)
import Data.Traversable (traverse)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param)
import Slack.Util (NetCtx, fromJSON, slackGetPaginated, slackPost)
import Prelude hiding (id)

data Channel = Channel
  { _id :: Text,
    _name :: Text,
    _topic :: Text
  }
  deriving (Generic, Show)

$(makeLenses ''Channel)

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \o -> do
    _id <- o .: "id"
    _name <- o .: "name"
    topicObject <- o .: "topic"
    _topic <- topicObject .: "value"
    return Channel {..}

findChannel :: NetCtx -> Text -> ExceptT Text IO (Maybe Channel)
findChannel netCtx expectedName = do
  let params =
        defaults & param "types" .~ ["public_channel,private_channel"]
          & param "exclude_archived" .~ ["true"]
          & param "limit" .~ ["1000"]
  respBodies <- slackGetPaginated netCtx params "conversations.list"
  channels <-
    traverse fromJSON
      . concatMap (^.. values)
      =<< (traverse (^? key "channels") respBodies ?? "\"users.list\" response didn't include a \"channels\" field")
  return $ find (\x -> (x ^. name) == expectedName) channels

createChannel :: NetCtx -> Text -> ExceptT Text IO Channel
createChannel netCtx newName = do
  let params = ["name" .= newName]
  respBody <- slackPost netCtx params "conversations.create"
  val <- (respBody ^? key "channel") ?? "\"conversations.create\" response didn't include a \"channel\" key"
  fromJSON val

setChannelTopic :: NetCtx -> Text -> Text -> ExceptT Text IO ()
setChannelTopic netCtx channelID newTopic = do
  let params =
        [ "channel" .= channelID,
          "topic" .= newTopic
        ]
  _ <- slackPost netCtx params "conversations.setTopic"
  return ()
