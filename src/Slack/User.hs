module Slack.User
  ( User (..),
    id,
    displayName,
    Users (..),
    Effects,
    find,
    listAll,
    runUsers,
  )
where

import Control.Lens ((&), (^.), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Lens (key, values)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, null)
import GHC.Generics (Generic)
import Network.Wreq (defaults)
import Polysemy (InterpreterFor, InterpretersFor, Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, note)
import Polysemy.State (State, evalState, get, put)
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
  Find :: Text -> Users m (Maybe User)
  ListAll :: Users m [User]

makeSem ''Users

type ID = Text

type UsersStore = (Bool, Map ID User)

type Effects =
  '[ Users,
     State UsersStore
   ]

runUsers :: Members '[Slack, Error Text] r => InterpretersFor Effects r
runUsers = evalState (False, Map.empty) . interpretWithCache
  where
    interpretWithCache :: Members '[Slack, Error Text, State UsersStore] r => InterpreterFor Users r
    interpretWithCache = interpret \case
      ListAll -> fmap snd . Map.toList <$> getAllThroughCache
      Find id -> Map.lookup id <$> getAllThroughCache
    getAllThroughCache :: Members '[Slack, Error Text, State UsersStore] r => Sem r (Map ID User)
    getAllThroughCache = do
      (alreadyRanListAll, map) <- get
      if alreadyRanListAll
        then return map
        else do
          users <- listAllUsers
          let newMap = Map.fromList ((\u -> (u ^. id, u)) <$> users)
          put (True, newMap)
          return newMap

listAllUsers :: Members '[Slack, Error Text] r => Sem r [User]
listAllUsers = do
  respBodies <- Slack.getPaginated defaults "users.list"
  vals <-
    concatMap (^.. values)
      <$> (traverse (^? key "members") respBodies & note "\"users.list\" response didn't include a \"members\" field")
  traverse fromJSON vals
