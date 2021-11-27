module Slack.Util
  ( slackGet,
    slackGetPaginated,
    slackPost,
    fromJSON,
    NetCtx (..),
    runNetCtx,
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
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Maybe (maybeToList)
import Data.Text as T (Text, null, pack)
import IO (Env, lookup)
import Network.Wreq (Options, Response, asValue, auth, defaults, oauth2Bearer, param, responseBody)
import Network.Wreq.Session (Session, getWith, newAPISession, postWith)
import Polysemy (Embed, Final, InterpreterFor, Member, Sem, embed, embedFinal, interpret)
import Polysemy.Error (Error, note)
import Polysemy.View (View (..), see)
import Prelude hiding (error, lookup)

data NetCtx = NetCtx ByteString Session

runNetCtx ::
  ( Member (Embed IO) r,
    Member Env r,
    Member (Error Text) r
  ) =>
  InterpreterFor (View NetCtx) r
runNetCtx program = do
  session <- embed newAPISession
  tokenM <- lookup "API_TOKEN"
  token <- BS.pack <$> note "API_TOKEN env variable not set" tokenM
  interpret (\case See -> return (NetCtx token session)) program

slackURL :: String -> String
slackURL = ("https://slack.com/api/" ++)

slackGet ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  Options ->
  String ->
  ExceptT Text (Sem r) Value
slackGet opts method = do
  (NetCtx apiToken sess) <- lift $ see @NetCtx
  let optsWithAuth = opts & auth ?~ oauth2Bearer apiToken
  let url = slackURL method
  resp <- lift $ embedFinal (asValue =<< getWith optsWithAuth sess url)
  hoistEither $ handleSlackError "GET" method resp

slackGetPaginated ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  Options ->
  String ->
  ExceptT Text (Sem r) [Value]
slackGetPaginated = slackGetPaginated' Nothing []

slackGetPaginated' ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  Maybe Text ->
  [Value] ->
  Options ->
  String ->
  ExceptT Text (Sem r) [Value]
slackGetPaginated' cursor !acc opts method = do
  let optsWithCursor = opts & param "cursor" .~ maybeToList cursor
  respBody <- slackGet optsWithCursor method
  let nextCursor = mfilter (not . T.null) $ respBody ^? key "response_metadata" . key "next_cursor" . _String
  let nextAcc = respBody : acc
  case nextCursor of
    Just _ -> slackGetPaginated' nextCursor nextAcc opts method
    Nothing -> return $ reverse nextAcc

slackPost ::
  ( Member (Final IO) r,
    Member (View NetCtx) r
  ) =>
  [Pair] ->
  String ->
  ExceptT Text (Sem r) Value
slackPost params method = do
  (NetCtx apiToken sess) <- lift $ see @NetCtx
  let optsWithAuth = defaults & auth ?~ oauth2Bearer apiToken
  let url = slackURL method
  let body = toJSON $ object params
  resp <- lift $ embedFinal $ asValue =<< postWith optsWithAuth sess url body
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
