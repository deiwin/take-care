module Slack.Util
  ( fromJSON,

    -- * Input NetCtx effect
    NetCtx,
    runNetCtx,

    -- * Slack API effect
    Slack (..),
    runSlack,
    get,
    getPaginated,
    post,
  )
where

import Control.Category ((>>>))
import Control.Concurrent (threadDelay)
import Control.Lens ((&), (.~), (?~), (^.), (^?), (^?!))
import Control.Monad (mfilter)
import Data.Aeson (FromJSON, Value, object, toJSON)
import Data.Aeson qualified as A (fromJSON)
import Data.Aeson.Lens (key, _Bool, _Integral, _String)
import Data.Aeson.Types (Pair, Result (Error, Success))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS (pack)
import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import Data.Text as T (Text, null, pack)
import IO (Env)
import IO qualified as Env (lookup)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.Wreq (Options, Response, asValue, auth, defaults, oauth2Bearer, param, responseBody, responseHeader, responseStatus, statusCode)
import Network.Wreq.Session (Session, getWith, newAPISession, postWith)
import Polysemy (Embed, InterpreterFor, Member, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error, catch, fromException, mapError, note, throw)
import Polysemy.Input (Input, input, runInputConst)
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log (warn)
import Text.Printf (printf)
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
  tokenM <- Env.lookup "SLACK_API_TOKEN"
  token <- BS.pack <$> note "SLACK_API_TOKEN env variable not set" tokenM
  runInputConst (NetCtx token session) program

data Slack m a where
  Get :: Options -> String -> Slack m Value
  GetPaginated :: Options -> String -> Slack m [Value]
  Post :: [Pair] -> String -> Slack m Value

makeSem ''Slack

runSlack ::
  ( Member (Embed IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r,
    Member Log r
  ) =>
  InterpreterFor Slack r
runSlack = interpret \case
  Get opts method -> slackGet opts method
  GetPaginated opts method -> slackGetPaginated opts method
  Post params method -> slackPost params method

slackURL :: String -> String
slackURL = ("https://slack.com/api/" ++)

slackGet ::
  ( Member (Embed IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r,
    Member Log r
  ) =>
  Options ->
  String ->
  Sem r Value
slackGet opts method = do
  (NetCtx apiToken sess) <- input @NetCtx
  let optsWithAuth = opts & auth ?~ oauth2Bearer apiToken
  let url = slackURL method
  resp <-
    getWith optsWithAuth sess url
      >>= asValue
      & retryRateLimit
  handleSlackError "GET" method resp

slackGetPaginated ::
  ( Member (Embed IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r,
    Member Log r
  ) =>
  Options ->
  String ->
  Sem r [Value]
slackGetPaginated = slackGetPaginated' Nothing []

slackGetPaginated' ::
  ( Member (Embed IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r,
    Member Log r
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
  ( Member (Embed IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r,
    Member Log r
  ) =>
  [Pair] ->
  String ->
  Sem r Value
slackPost params method = do
  (NetCtx apiToken sess) <- input @NetCtx
  let optsWithAuth = defaults & auth ?~ oauth2Bearer apiToken
  let url = slackURL method
  let body = toJSON $ object params
  resp <-
    postWith optsWithAuth sess url body
      >>= asValue
      & retryRateLimit
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

retryRateLimit ::
  ( Member (Embed IO) r,
    Member (Error Text) r,
    Member Log r
  ) =>
  IO (Response a) ->
  Sem r (Response a)
retryRateLimit = go (replicate 4 ()) -- 4 retries, meaning 5 tries in total
  where
    go [] requestM =
      requestM
        & fromException
        & mapError httpToText
    go (_ : rest) requestM = do
      requestM
        & fromException
        & flip catch (catchRateLimited rest requestM)
        & mapError httpToText

    httpToText :: HttpException -> Text
    httpToText = T.pack . show

    catchRateLimited ::
      ( Member (Embed IO) r,
        Member (Error Text) r,
        Member (Error HttpException) r,
        Member Log r
      ) =>
      [()] ->
      IO (Response a) ->
      HttpException ->
      Sem r (Response a)
    catchRateLimited rest requestM e@(HttpExceptionRequest _ (StatusCodeException resp _body)) = do
      if isRateLimited resp
        then do
          duration <- waitDuration resp
          Log.warn (T.pack (printf "The request was ratelimited. Waiting %d seconds before trying again .." duration))
          wait duration
          go rest requestM
        else throw e
    catchRateLimited _ _ e = throw e

    isRateLimited r = (r ^. responseStatus . statusCode) == 429
    wait durationSeconds = embed $ threadDelay (durationSeconds * 1000000)
    waitDuration r =
      retryAfter r
        & note "Expected a Retry-After header but could not find it."
        <&> (+ 5) -- A small buffer
    retryAfter r = r ^? responseHeader "Retry-After" . _Integral

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
