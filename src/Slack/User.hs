module Slack.User
  ( User (..),
    id,
    displayName,
    email,
    Users (..),
    Effects,
    find,
    listAll,
    runUsers,
  )
where

import Control.Applicative (liftA2)
import Control.Lens (has, (&), (^.), (^..), (^?))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Lens (key, values, _Object)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text as T (Text, null)
import GHC.Generics (Generic)
import Network.Wreq (defaults)
import Polysemy (InterpreterFor, InterpretersFor, Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, note)
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log (info)
import Polysemy.State (State, evalState, get, put)
import Slack.Util (Slack, fromJSON)
import qualified Slack.Util as Slack (getPaginated)
import Text.Printf (printf)
import Prelude hiding (id)

data User = User
  { _id :: Text,
    _displayName :: Text,
    _email :: Text
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

    _email <- profile .: "email"

    return User {..}

data Users m a where
  Find :: Text -> Users m (Maybe User)
  ListAll :: Users m [User]

makeSem ''Users

type Email = Text

type UsersStore = (Bool, Map Email User)

type Effects =
  '[ Users,
     State UsersStore
   ]

runUsers :: Members '[Slack, Error Text, Log] r => InterpretersFor Effects r
runUsers = evalState (False, Map.empty) . interpretWithCache
  where
    interpretWithCache :: Members '[Slack, Error Text, State UsersStore, Log] r => InterpreterFor Users r
    interpretWithCache = interpret \case
      ListAll -> do
        Log.info "Listing all users .."
        fmap snd . Map.toList <$> getAllThroughCache
      Find email -> do
        Log.info (pack (printf "Finding user with email %s .." email))
        Map.lookup email <$> getAllThroughCache
    getAllThroughCache :: Members '[Slack, Error Text, State UsersStore, Log] r => Sem r (Map Email User)
    getAllThroughCache = do
      (alreadyRanListAll, map) <- get
      if alreadyRanListAll
        then do
          Log.info "Using cached user map .."
          return map
        else do
          Log.info "Building user cache .."
          users <- listAllUsers
          let newMap = Map.fromList ((\u -> (u ^. email, u)) <$> users)
          put (True, newMap)
          return newMap

listAllUsers :: Members '[Slack, Error Text] r => Sem r [User]
listAllUsers = do
  respBodies <- Slack.getPaginated defaults "users.list"
  memberBodies <-
    respBodies
      & traverse (^? key "members")
      & note "\"users.list\" response didn't include a \"members\" field"
  memberBodies
    & concatMap (^.. values)
    & filter (not . profileWithoutEmail)
    & traverse fromJSON
  where
    profileWithoutEmail =
      has (key "profile" . _Object)
        <&&> (not . has (key "profile" . key "email"))

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
