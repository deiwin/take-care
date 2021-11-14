{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slack.Util
  ( slackGet,
    slackGetPaginated,
    slackPost,
    fromJSON,
    NetCtx (..),
  )
where

import Control.Error.Util (hoistEither)
import Control.Lens ((&), (.~), (?~), (^.), (^?), (^?!))
import Control.Monad (mfilter)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON, Value, object, toJSON)
import qualified Data.Aeson as A (fromJSON)
import Data.Aeson.Lens (key, _Bool, _String)
import Data.Aeson.Types (Pair, Result (Error, Success))
import Data.ByteString (ByteString)
import Data.Maybe (maybeToList)
import Data.Text as T (Text, null, pack)
import Network.Wreq (Options, Response, asValue, auth, defaults, oauth2Bearer, param, responseBody)
import Network.Wreq.Session (Session, getWith, postWith)
import Prelude hiding (error)

data NetCtx = NetCtx ByteString Session

slackURL :: String -> String
slackURL = ("https://slack.com/api/" ++)

slackGet :: NetCtx -> Options -> String -> ExceptT Text IO Value
slackGet (NetCtx apiToken sess) opts method = do
  let optsWithAuth = opts & auth ?~ oauth2Bearer apiToken
  let url = slackURL method
  resp <- lift (asValue =<< getWith optsWithAuth sess url)
  hoistEither $ handleSlackError "GET" method resp

slackGetPaginated :: NetCtx -> Options -> String -> ExceptT Text IO [Value]
slackGetPaginated = slackGetPaginated' Nothing []

slackGetPaginated' :: Maybe Text -> [Value] -> NetCtx -> Options -> String -> ExceptT Text IO [Value]
slackGetPaginated' cursor !acc netCtx opts method = do
  let optsWithCursor = opts & param "cursor" .~ maybeToList cursor
  respBody <- slackGet netCtx optsWithCursor method
  let nextCursor = mfilter (not . T.null) $ respBody ^? key "response_metadata" . key "next_cursor" . _String
  let nextAcc = respBody : acc
  case nextCursor of
    Just _ -> slackGetPaginated' nextCursor nextAcc netCtx opts method
    Nothing -> return $ reverse nextAcc

slackPost :: NetCtx -> [Pair] -> String -> ExceptT Text IO Value
slackPost (NetCtx apiToken sess) params method = do
  let optsWithAuth = defaults & auth ?~ oauth2Bearer apiToken
  let url = slackURL method
  let body = toJSON $ object params
  resp <- lift $ asValue =<< postWith optsWithAuth sess url body
  hoistEither $ handleSlackError "POST" method resp

handleSlackError :: Text -> String -> Response Value -> Either Text Value
handleSlackError httpMethod method resp =
  let respBody = resp ^. responseBody
      ok = respBody ^?! key "ok" . _Bool
      error = respBody ^. key "error" . _String
      detail = respBody ^? key "detail" . _String
   in if ok
        then Right respBody
        else Left (httpMethod <> " " <> T.pack method <> ": " <> error <> maybe "" (" - " <>) detail)

fromJSON :: (FromJSON a, Monad m) => Value -> ExceptT Text m a
fromJSON = hoistEither . hoistResult . A.fromJSON
  where
    hoistResult res = case res of
      Error e -> Left $ pack e
      Success o -> Right o
