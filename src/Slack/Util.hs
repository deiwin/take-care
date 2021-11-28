module Slack.Util
  ( slackGet,
    slackGetPaginated,
    slackPost,
    fromJSON,
    NetCtx (..),
    runNetCtx,
  )
where

import Control.Category ((>>>))
import Control.Lens ((&), (.~), (?~), (^.), (^?), (^?!))
import Control.Monad (mfilter)
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
import Polysemy (Embed, Final, InterpreterFor, Member, Sem, embed, embedFinal)
import Polysemy.Error (Error, note, throw)
import Polysemy.Input (Input, input, runInputConst)
import Prelude hiding (lookup)

data NetCtx = NetCtx ByteString Session

runNetCtx ::
  ( Member (Embed IO) r,
    Member Env r,
    Member (Error Text) r
  ) =>
  InterpreterFor (Input NetCtx) r
runNetCtx program = do
  session <- embed newAPISession
  tokenM <- lookup "API_TOKEN"
  token <- BS.pack <$> note "API_TOKEN env variable not set" tokenM
  runInputConst (NetCtx token session) program

slackURL :: String -> String
slackURL = ("https://slack.com/api/" ++)

slackGet ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  Options ->
  String ->
  Sem r Value
slackGet opts method = do
  (NetCtx apiToken sess) <- input @NetCtx
  let optsWithAuth = opts & auth ?~ oauth2Bearer apiToken
  let url = slackURL method
  resp <- embedFinal (asValue =<< getWith optsWithAuth sess url)
  handleSlackError "GET" method resp

slackGetPaginated ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  Options ->
  String ->
  Sem r [Value]
slackGetPaginated = slackGetPaginated' Nothing []

slackGetPaginated' ::
  ( Member (Final IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  Maybe Text ->
  [Value] ->
  Options ->
  String ->
  Sem r [Value]
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
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  [Pair] ->
  String ->
  Sem r Value
slackPost params method = do
  (NetCtx apiToken sess) <- input @NetCtx
  let optsWithAuth = defaults & auth ?~ oauth2Bearer apiToken
  let url = slackURL method
  let body = toJSON $ object params
  resp <- embedFinal $ asValue =<< postWith optsWithAuth sess url body
  handleSlackError "POST" method resp

handleSlackError ::
  Member (Error Text) r =>
  Text ->
  String ->
  Response Value ->
  Sem r Value
handleSlackError httpMethod method resp =
  let respBody = resp ^. responseBody
      ok = respBody ^?! key "ok" . _Bool
      errorMessage = respBody ^. key "error" . _String
      detail = respBody ^? key "detail" . _String
   in if ok
        then return respBody
        else throw (httpMethod <> " " <> T.pack method <> ": " <> errorMessage <> maybe "" (" - " <>) detail)

fromJSON ::
  ( FromJSON a,
    Member (Error Text) r
  ) =>
  Value ->
  Sem r a
fromJSON =
  A.fromJSON >>> \case
    Error e -> throw $ pack e
    Success o -> return o
