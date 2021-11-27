module Slack.Channel
  ( findChannel,
    createChannel,
    setChannelTopic,
    Channel,
    id,
    topic,
  )
where

import Control.Lens ((&), (.~), (^.), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.=))
import Data.Aeson.Lens (key, values)
import Data.List as L (find)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param)
import Polysemy (Final, Member, Sem)
import Polysemy.Error (Error, note)
import Polysemy.View (View)
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

findChannel ::
  ( Member (Final IO) r,
    Member (View NetCtx) r,
    Member (Error Text) r
  ) =>
  Text ->
  Sem r (Maybe Channel)
findChannel expectedName = do
  let params =
        defaults & param "types" .~ ["public_channel,private_channel"]
          & param "exclude_archived" .~ ["true"]
          & param "limit" .~ ["1000"]
  respBodies <- slackGetPaginated params "conversations.list"
  channels <-
    traverse fromJSON
      . concatMap (^.. values)
      =<< (traverse (^? key "channels") respBodies & note "\"users.list\" response didn't include a \"channels\" field")
  return $ find (\x -> (x ^. name) == expectedName) channels

createChannel ::
  ( Member (Final IO) r,
    Member (View NetCtx) r,
    Member (Error Text) r
  ) =>
  Text ->
  Sem r Channel
createChannel newName = do
  let params = ["name" .= newName]
  respBody <- slackPost params "conversations.create"
  val <- (respBody ^? key "channel") & note "\"conversations.create\" response didn't include a \"channel\" key"
  fromJSON val

setChannelTopic ::
  ( Member (Final IO) r,
    Member (View NetCtx) r,
    Member (Error Text) r
  ) =>
  Text ->
  Text ->
  Sem r ()
setChannelTopic channelID newTopic = do
  let params =
        [ "channel" .= channelID,
          "topic" .= newTopic
        ]
  _ <- slackPost params "conversations.setTopic"
  return ()
