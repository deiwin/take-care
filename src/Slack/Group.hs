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

import Control.Lens ((&), (.~), (^.), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.=))
import Data.Aeson.Lens (key, values, _String)
import Data.List (find, intercalate)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param)
import Polysemy (Final, Member, Sem)
import Polysemy.Error (Error, note)
import Polysemy.View (View)
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

getGroupMembers ::
  ( Member (Final IO) r,
    Member (View NetCtx) r,
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
    Member (View NetCtx) r,
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
    Member (View NetCtx) r,
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
    Member (View NetCtx) r,
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
  return $ find (\x -> (x ^. handle) == expectedHandle) groups

createGroup ::
  ( Member (Final IO) r,
    Member (View NetCtx) r,
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
