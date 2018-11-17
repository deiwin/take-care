{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.User
    ( getDisplayName
    , getAllDisplayNames
    ) where

import Slack.Util (slackGet, slackGetPaginated, fromJSON, Token)
import Data.Text (Text)
import Data.Traversable (traverse)
import Network.Wreq (defaults, param)
import Control.Lens ((&), (.~), (^?), (^.), (^..))
import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(parseJSON), withObject, (.:))
import Data.Aeson.Lens (key, values, _String)
import Data.List (zip)
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

-- TODO return User
getDisplayName :: Token -> Text -> ExceptT Text IO Text
getDisplayName apiToken userID = do
    let opts = defaults & param "user" .~ [userID]
    respBody <- slackGet apiToken opts "users.info"
    val <- (respBody ^? key "user") ?? "\"users.info\" response didn't include a \"user\" field"
    user <- fromJSON val
    return (user ^. displayName)

-- TODO return [User]
getAllDisplayNames :: Token -> ExceptT Text IO [(Text, Text)]
getAllDisplayNames apiToken = do
    respBodies <- slackGetPaginated apiToken defaults "users.list"
    let members = respBodies ^.. traverse . key "members" . values
    let ids = members ^.. traverse . key "id" . _String
    let displayNames = ("@" <>) <$> members ^.. traverse . key "profile" . key "display_name" . _String
    return $ zip ids displayNames
