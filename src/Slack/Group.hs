module Slack.Group
  ( Group,
    id,
    handle,
    channelIDs,
    Groups,
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
import qualified Data.List as L (find)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param)
import Polysemy (Final, InterpreterFor, Member, Sem, interpret, makeSem)
import Polysemy.Error (Error, note)
import Polysemy.Input (Input)
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

data Groups m a where
  GetMembers :: Text -> Groups m [Text]
  SetMembers :: Text -> [Text] -> Groups m ()
  SetChannels :: Text -> [Text] -> Groups m ()
  Find :: Text -> Groups m (Maybe Group)
  Create :: Text -> Text -> [Text] -> Groups m Group

makeSem ''Groups

runGroups ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  InterpreterFor Groups r
runGroups = interpret \case
  GetMembers id -> getGroupMembers id
  SetMembers groupID userIDs -> setGroupMembers groupID userIDs
  SetChannels groupID defaultChannelIDs -> setGroupChannels groupID defaultChannelIDs
  Find handle -> findGroup handle
  Create handle name defaultChannelIDs -> createGroup handle name defaultChannelIDs

getGroupMembers ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  Text ->
  Sem r [Text]
getGroupMembers groupID = do
  let opts =
        defaults & param "usergroup" .~ [groupID]
          & param "include_disabled" .~ ["true"]
  respBody <- slackGet opts "usergroups.users.list"
  return $ respBody ^.. key "users" . values . _String

setGroupMembers ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  Text ->
  [Text] ->
  Sem r ()
setGroupMembers groupID userIDs = do
  let params =
        [ "usergroup" .= groupID,
          "users" .= intercalate "," (unpack <$> userIDs)
        ]
  _ <- slackPost params "usergroups.users.update"
  return ()

setGroupChannels ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  Text ->
  [Text] ->
  Sem r ()
setGroupChannels groupID defaultChannelIDs = do
  let params =
        [ "usergroup" .= groupID,
          "channels" .= intercalate "," (unpack <$> defaultChannelIDs)
        ]
  _ <- slackPost params "usergroups.update"
  return ()

findGroup ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  Text ->
  Sem r (Maybe Group)
findGroup expectedHandle = do
  let opts = defaults & param "include_disabled" .~ ["true"]
  respBody <- slackGet opts "usergroups.list"
  groups <-
    traverse fromJSON
      . (^.. values)
      =<< ((respBody ^? key "usergroups") & note "\"users.list\" response didn't include a \"channels\" field")
  return $ L.find (\x -> (x ^. handle) == expectedHandle) groups

createGroup ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  Text ->
  Text ->
  [Text] ->
  Sem r Group
createGroup groupHandle groupName defaultChannelIDs = do
  let params =
        [ "handle" .= groupHandle,
          "name" .= groupName,
          "channels" .= intercalate "," (unpack <$> defaultChannelIDs)
        ]
  respBody <- slackPost params "usergroups.create"
  val <- (respBody ^? key "usergroup") & note "\"usergroups.create\" response didn't include a \"usergroup\" key"
  fromJSON val
