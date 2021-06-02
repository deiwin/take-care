{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.User
    ( getUser
    , listAllUsers
    , User
    , id
    , displayName
    )
where

import Prelude hiding (id)
import Slack.Util (slackGet, slackGetPaginated, fromJSON, NetCtx)
import Data.Text as T (Text, null)
import Data.Traversable (traverse)
import Network.Wreq (defaults, param)
import Control.Lens ((&), (.~), (^?), (^..))
import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Lens (key, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Trans.Except (ExceptT)
import Control.Error.Util ((??))

data User = User { _id :: Text
                 , _displayName :: Text
                 } deriving (Generic, Show)
$(makeLenses ''User)

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> do
        _id <- o .: "id"
        name <- o .: "name"

        profile <- o .: "profile"
        displayName <- profile .:? "display_name"
        let nonEmptyDisplayName = displayName >>= (\x -> if T.null x then Nothing else Just x)

        let _displayName = "@" <> fromMaybe name nonEmptyDisplayName
        return User{..}

getUser :: NetCtx -> Text -> ExceptT Text IO User
getUser netCtx userID = do
    let opts = defaults & param "user" .~ [userID]
    respBody <- slackGet netCtx opts "users.info"
    val      <- (respBody ^? key "user") ?? "\"users.info\" response didn't include a \"user\" field"
    fromJSON val

listAllUsers :: NetCtx -> ExceptT Text IO [User]
listAllUsers netCtx = do
    respBodies <- slackGetPaginated netCtx defaults "users.list"
    vals       <-
        concatMap (^.. values)
            <$> (traverse (^? key "members") respBodies ?? "\"users.list\" response didn't include a \"members\" field")
    traverse fromJSON vals
