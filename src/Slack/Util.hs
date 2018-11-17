{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}

module Slack.Util
    ( slackGet
    , slackGetPaginated
    , slackPost
    , Token
    ) where

import Prelude hiding (error)
import Data.Text as T (Text, pack, null)
import Data.ByteString (ByteString)
import Control.Monad (mfilter)
import Network.Wreq (defaults, param, getWith, postWith, asValue, responseBody, auth
  , oauth2Bearer, Options, Response)
import Data.Maybe (maybeToList)
import Data.Aeson (toJSON, object, Value)
import Data.Aeson.Types (Pair)
import Control.Lens ((&), (.~), (^?), (^?!), (^.), (?~))
import Data.Aeson.Lens (key, _String, _Bool)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Error.Util (hoistEither)

type Token = ByteString

slackURL :: String -> String
slackURL = ("https://slack.com/api/" ++)

slackGet :: Token -> Options -> String -> ExceptT Text IO Value
slackGet apiToken opts method = do
    let optsWithAuth = opts & auth ?~ oauth2Bearer apiToken
    let url = slackURL method
    resp <- lift (asValue =<< getWith optsWithAuth url)
    hoistEither $ handleSlackError "GET" method resp

slackGetPaginated :: Token -> Options -> String -> ExceptT Text IO [Value]
slackGetPaginated = slackGetPaginated' Nothing []
slackGetPaginated' :: Maybe Text -> [Value] -> Token -> Options -> String -> ExceptT Text IO [Value]
slackGetPaginated' cursor !acc apiToken opts method = do
    let optsWithCursor = opts & param "cursor" .~ maybeToList cursor
    respBody <- slackGet apiToken optsWithCursor method
    let
      nextCursor = mfilter (not . T.null) $
          respBody ^? key "response_metadata" . key "next_cursor" . _String
    let nextAcc = respBody : acc
    case nextCursor of
      Just _ -> slackGetPaginated' nextCursor nextAcc apiToken opts method
      Nothing -> return $ reverse nextAcc

slackPost :: Token -> [Pair] -> String -> ExceptT Text IO Value
slackPost apiToken params method = do
    let optsWithAuth = defaults & auth ?~ oauth2Bearer apiToken
    let url = slackURL method
    let body = toJSON $ object params
    resp <- lift $ asValue =<< postWith optsWithAuth url body
    hoistEither $ handleSlackError "POST" method resp

handleSlackError :: Text -> String -> Response Value -> Either Text Value
handleSlackError httpMethod method resp =
    let respBody = resp ^. responseBody
        ok = respBody ^?! key "ok" . _Bool
        error = respBody ^. key "error" . _String
        detail = respBody ^? key "detail" . _String
     in if ok then
            Right respBody
        else
            Left (httpMethod <> " " <> T.pack method <> ": " <> error <> maybe "" (" - " <>) detail)
