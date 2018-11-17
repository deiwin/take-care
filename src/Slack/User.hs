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
    ) where

import Prelude hiding (id)
import Slack.Util (slackGet, slackGetPaginated, fromJSON, Token)
import Data.Text (Text)
import Data.Traversable (traverse)
import Network.Wreq (defaults, param)
import Control.Lens ((&), (.~), (^?), (^..))
import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(parseJSON), withObject, (.:))
import Data.Aeson.Lens (key, values)
import Control.Monad.Trans.Except (ExceptT)
import Control.Error.Util ((??))

data User = User { _id :: Text
                 , _displayName :: Text
                 } deriving (Generic, Show)
$(makeLenses ''User)

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> do
        _id <- o .: "id"
        profile <- o .: "profile"
        _displayName <- ("@" <>) <$> (profile .: "display_name")
        return User{..}

getUser :: Token -> Text -> ExceptT Text IO User
getUser apiToken userID = do
    let opts = defaults & param "user" .~ [userID]
    respBody <- slackGet apiToken opts "users.info"
    val <- (respBody ^? key "user") ?? "\"users.info\" response didn't include a \"user\" field"
    fromJSON val

listAllUsers :: Token -> ExceptT Text IO [User]
listAllUsers apiToken = do
    respBodies <- slackGetPaginated apiToken defaults "users.list"
    vals <- concatMap (^.. values) <$>
        (traverse (^? key "members") respBodies ??
            "\"users.list\" response didn't include a \"members\" field")
    traverse fromJSON vals
