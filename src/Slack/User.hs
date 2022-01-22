module Slack.User
  ( User (..),
    id,
    displayName,
    Users,
    listAll,
    runUsers,
  )
where

import Control.Lens ((&), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Lens (key, values)
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, null)
import GHC.Generics (Generic)
import Network.Wreq (defaults)
import Polysemy (InterpreterFor, Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, note)
import Slack.Util (Slack, fromJSON)
import qualified Slack.Util as Slack (getPaginated)
import Prelude hiding (id)

data User = User
  { _id :: Text,
    _displayName :: Text
  }
  deriving (Generic, Show, Eq)

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

data Users m a where
  ListAll :: Users m [User]

makeSem ''Users

runUsers :: Members '[Slack, Error Text] r => InterpreterFor Users r
runUsers = interpret \case
  ListAll -> listAllUsers

listAllUsers :: Members '[Slack, Error Text] r => Sem r [User]
listAllUsers = do
  respBodies <- Slack.getPaginated defaults "users.list"
  vals <-
    concatMap (^.. values)
      <$> (traverse (^? key "members") respBodies & note "\"users.list\" response didn't include a \"members\" field")
  traverse fromJSON vals
