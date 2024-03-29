module Slack.Group
  ( Group (..),
    id,
    handle,
    name,
    channelIDs,
    Groups (..),
    runGroups,
    getMembers,
    setMembers,
    setChannels,
    find,
    create,
  )
where

import Control.Lens ((&), (.~), (^.), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.=))
import Data.Aeson.Lens (key, values, _String)
import Data.List (intercalate)
import Data.List qualified as L (find)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param)
import Polysemy (InterpreterFor, Member, Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, note)
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log (info)
import Slack.Util (Slack, fromJSON)
import Slack.Util qualified as Slack (get, post)
import Text.Printf (printf)
import Prelude hiding (id)

data Group = Group
  { _id :: Text,
    _handle :: Text,
    _name :: Text,
    _channelIDs :: [Text]
  }
  deriving (Generic, Show, Eq)

$(makeLenses ''Group)

instance FromJSON Group where
  parseJSON = withObject "Group" $ \o -> do
    _id <- o .: "id"
    _handle <- o .: "handle"
    _name <- o .: "name"
    prefs <- o .: "prefs"
    _channelIDs <- prefs .: "channels"
    return Group {..}

data Groups m a where
  GetMembers :: Text -> Groups m [Text]
  SetMembers :: Text -> [Text] -> Groups m ()
  SetChannels :: Text -> [Text] -> Groups m ()
  Find :: Text -> Groups m (Maybe Group)
  Create :: Text -> Text -> [Text] -> Groups m Group

makeSem ''Groups

runGroups :: Members '[Slack, Error Text, Log] r => InterpreterFor Groups r
runGroups = interpret \case
  GetMembers id -> do
    Log.info (pack (printf "Getting members of group with ID %s .." id))
    getGroupMembers id
  SetMembers groupID userIDs -> do
    Log.info (pack (printf "Setting members of group with ID %s to user IDs %s .." groupID (show userIDs)))
    setGroupMembers groupID userIDs
  SetChannels groupID defaultChannelIDs -> do
    Log.info (pack (printf "Setting channels of group with ID %s to channel IDs %s .." groupID (show defaultChannelIDs)))
    setGroupChannels groupID defaultChannelIDs
  Find handle -> do
    Log.info (pack (printf "Finding group by handle @%s .." handle))
    findGroup handle
  Create handle name defaultChannelIDs -> do
    Log.info (pack (printf "Creating a group with handle @%s .." handle))
    createGroup handle name defaultChannelIDs

getGroupMembers :: Member Slack r => Text -> Sem r [Text]
getGroupMembers groupID = do
  let opts =
        defaults
          & param "usergroup" .~ [groupID]
          & param "include_disabled" .~ ["true"]
  respBody <- Slack.get opts "usergroups.users.list"
  return $ respBody ^.. key "users" . values . _String

setGroupMembers :: Member Slack r => Text -> [Text] -> Sem r ()
setGroupMembers groupID userIDs = do
  let params =
        [ "usergroup" .= groupID,
          "users" .= intercalate "," (unpack <$> userIDs)
        ]
  _ <- Slack.post params "usergroups.users.update"
  return ()

setGroupChannels :: Member Slack r => Text -> [Text] -> Sem r ()
setGroupChannels groupID defaultChannelIDs = do
  let params =
        [ "usergroup" .= groupID,
          "channels" .= intercalate "," (unpack <$> defaultChannelIDs)
        ]
  _ <- Slack.post params "usergroups.update"
  return ()

findGroup :: Members '[Slack, Error Text] r => Text -> Sem r (Maybe Group)
findGroup expectedHandle = do
  let opts = defaults & param "include_disabled" .~ ["true"]
  respBody <- Slack.get opts "usergroups.list"
  groupListBody <-
    respBody
      & (^? key "usergroups")
      & note "\"usergroups.list\" response didn't include a \"usergroups\" field"
  groupListBody
    & (^.. values)
    & traverse fromJSON
    & fmap (L.find (\x -> (x ^. handle) == expectedHandle))

createGroup :: Members '[Slack, Error Text] r => Text -> Text -> [Text] -> Sem r Group
createGroup groupHandle groupName defaultChannelIDs = do
  let params =
        [ "handle" .= groupHandle,
          "name" .= groupName,
          "channels" .= intercalate "," (unpack <$> defaultChannelIDs)
        ]
  respBody <- Slack.post params "usergroups.create"
  val <- (respBody ^? key "usergroup") & note "\"usergroups.create\" response didn't include a \"usergroup\" key"
  fromJSON val
