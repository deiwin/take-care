module Slack.User
  ( getUser,
    listAllUsers,
    User,
    id,
    displayName,
  )
where

import Control.Error.Util ((??))
import Control.Lens ((&), (.~), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Lens (key, values)
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, null)
import GHC.Generics (Generic)
import Network.Wreq (defaults, param)
import Polysemy (Final, Member, Sem)
import Polysemy.View (View)
import Slack.Util (NetCtx, fromJSON, slackGet, slackGetPaginated)
import Prelude hiding (id)

data User = User
  { _id :: Text,
    _displayName :: Text
  }
  deriving (Generic, Show)

$(makeLenses ''User)

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    _id <- o .: "id"
    name <- o .: "name"

    profile <- o .: "profile"
    displayName <- profile .:? "display_name"
    let nonEmptyDisplayName = displayName >>= (\x -> if T.null x then Nothing else Just x)

    let _displayName = "@" <> fromMaybe name nonEmptyDisplayName
    return User {..}

getUser ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  Text ->
  ExceptT Text (Sem r) User
getUser userID = do
  let opts = defaults & param "user" .~ [userID]
  respBody <- slackGet opts "users.info"
  val <- (respBody ^? key "user") ?? "\"users.info\" response didn't include a \"user\" field"
  fromJSON val

listAllUsers ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  ExceptT Text (Sem r) [User]
listAllUsers = do
  respBodies <- slackGetPaginated defaults "users.list"
  vals <-
    concatMap (^.. values)
      <$> (traverse (^? key "members") respBodies ?? "\"users.list\" response didn't include a \"members\" field")
  traverse fromJSON vals
