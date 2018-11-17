{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Slack.User
    ( getDisplayName
    , getAllDisplayNames
    ) where

import Slack.Util (slackGet, slackGetPaginated, Token)
import Data.Text (Text)
import Data.Traversable (traverse)
import Network.Wreq (defaults, param)
import Control.Lens ((&), (.~), (^?), (^..))
import Data.Aeson.Lens (key, values, _String)
import Data.List (zip)
import Control.Monad.Trans.Except (ExceptT)
import Control.Error.Util ((??))

getDisplayName :: Token -> Text -> ExceptT Text IO Text
getDisplayName apiToken userID = do
    let opts = defaults & param "user" .~ [userID]
    respBody <- slackGet apiToken opts "users.info"
    let displayName = ("@" <>) <$> respBody ^? key "user" . key "profile" . key "display_name" . _String
    displayName ?? "\"users.info\" response didn't include \"user.profile.display_name\" field"

getAllDisplayNames :: Token -> ExceptT Text IO [(Text, Text)]
getAllDisplayNames apiToken = do
    respBodies <- slackGetPaginated apiToken defaults "users.list"
    let members = respBodies ^.. traverse . key "members" . values
    let ids = members ^.. traverse . key "id" . _String
    let displayNames = ("@" <>) <$> members ^.. traverse . key "profile" . key "display_name" . _String
    return $ zip ids displayNames
