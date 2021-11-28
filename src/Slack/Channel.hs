module Slack.Channel
  ( Channel,
    id,
    topic,
    Channels,
    create,
    find,
    setTopic,
    runChannels,
  )
where

import Control.Lens ((&), (.~), (^.), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.=))
import Data.Aeson.Lens (key, values)
import qualified Data.List as L (find)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param)
import Polysemy (Final, InterpreterFor, Member, Sem, interpret, makeSem)
import Polysemy.Error (Error, note)
import Polysemy.Input (Input)
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

data Channels m a where
  Find :: Text -> Channels m (Maybe Channel)
  Create :: Text -> Channels m Channel
  SetTopic :: Text -> Text -> Channels m ()

makeSem ''Channels

runChannels ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  InterpreterFor Channels r
runChannels = interpret \case
  Find name -> findChannel name
  Create name -> createChannel name
  SetTopic id topic -> setChannelTopic id topic

findChannel ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
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
  return $ L.find (\x -> (x ^. name) == expectedName) channels

createChannel ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
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
    Member (Input NetCtx) r,
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
