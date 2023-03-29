module Slack.Channel
  ( Channel (..),
    id,
    name,
    topic,
    Channels (..),
    Effects,
    create,
    setTopic,
    find,
    runChannels,
  )
where

import Control.Lens ((&), (.~), (^.), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.=))
import Data.Aeson.Lens (key, values)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param)
import Polysemy (InterpreterFor, InterpretersFor, Member, Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, note)
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log (info)
import Polysemy.State (State, evalState, get, modify, put)
import Slack.Util (Slack, fromJSON)
import Slack.Util qualified as Slack (getPaginated, post)
import Text.Printf (printf)
import Prelude hiding (id)

data Channel = Channel
  { _id :: Text,
    _name :: Text,
    _topic :: Text
  }
  deriving (Generic, Show, Eq)

$(makeLenses ''Channel)

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \o -> do
    _id <- o .: "id"
    _name <- o .: "name"
    topicObject <- o .: "topic"
    _topic <- topicObject .: "value"
    return Channel {..}

data Channels m a where
  Create :: Text -> Channels m Channel
  SetTopic :: Text -> Text -> Channels m ()
  Find :: Text -> Channels m (Maybe Channel)

makeSem ''Channels

type ID = Text

type Name = Text

type ChannelStore = (Bool, Map Name ID, Map ID Channel)

type Effects =
  '[ Channels,
     State ChannelStore
   ]

runChannels :: Members '[Slack, Error Text, Log] r => InterpretersFor Effects r
runChannels = evalState (False, Map.empty, Map.empty) . interpretWithCache
  where
    interpretWithCache :: Members '[Slack, Error Text, State ChannelStore, Log] r => InterpreterFor Channels r
    interpretWithCache = interpret \case
      Create name -> do
        Log.info (pack (printf "Creating channel #%s .." name))
        channel <- createChannel name
        Log.info (pack (printf "Finished creating #%s. Updating channel cache .." name))
        modify $ second3 $ Map.insert name (channel ^. id)
        modify $ third3 $ Map.insert (channel ^. id) channel
        return channel
      SetTopic id newTopic -> do
        Log.info (pack (printf "Setting channel (ID: %s) topic to: %s .." id newTopic))
        setChannelTopic id newTopic
        Log.info (pack (printf "Finished setting channel (ID: %s) topic. Updating channel cache .." id))
        modify $ third3 $ Map.adjust (topic .~ newTopic) id
      Find nameToFind -> do
        Log.info (pack (printf "Finding channel #%s .." nameToFind))
        (alreadyRanListAll, nameIx, map) <- get
        if alreadyRanListAll
          then do
            Log.info (pack (printf "Using channel cache to find channel #%s .." nameToFind))
            return $ findByName nameIx map nameToFind
          else do
            Log.info (pack (printf "Building channel cache before finding channel #%s .." nameToFind))
            channels <- listAllChannels
            let newNameIx = Map.union (Map.fromList ((\c -> (c ^. name, c ^. id)) <$> channels)) nameIx
            let newMap = Map.union (Map.fromList ((\c -> (c ^. id, c)) <$> channels)) map
            put (True, newNameIx, newMap)
            Log.info (pack (printf "Finished building channel cache. Finding channel #%s .." nameToFind))
            return $ findByName newNameIx newMap nameToFind
        where
          findByName :: Map Name ID -> Map ID Channel -> Name -> Maybe Channel
          findByName nameIx map name = do
            id <- Map.lookup name nameIx
            Map.lookup id map

second3 f (a, b, c) = (a, f b, c)

third3 f (a, b, c) = (a, b, f c)

listAllChannels :: Members '[Slack, Error Text] r => Sem r [Channel]
listAllChannels = do
  let params =
        defaults
          & param "types" .~ ["public_channel,private_channel"]
          & param "exclude_archived" .~ ["true"]
          & param "limit" .~ ["1000"]
  respBodies <- Slack.getPaginated params "conversations.list"
  channelBodies <-
    respBodies
      & traverse (^? key "channels")
      & note "\"conversations.list\" response didn't include a \"channels\" field"
  channelBodies
    & concatMap (^.. values)
    & traverse fromJSON

createChannel :: Members '[Slack, Error Text] r => Text -> Sem r Channel
createChannel newName = do
  let params = ["name" .= newName]
  respBody <- Slack.post params "conversations.create"
  val <- (respBody ^? key "channel") & note "\"conversations.create\" response didn't include a \"channel\" key"
  fromJSON val

setChannelTopic :: Member Slack r => Text -> Text -> Sem r ()
setChannelTopic channelID newTopic = do
  let params =
        [ "channel" .= channelID,
          "topic" .= newTopic
        ]
  _ <- Slack.post params "conversations.setTopic"
  return ()
