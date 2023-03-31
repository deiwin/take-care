module Opsgenie
  ( Opsgenie (..),
    whoIsOnCall,
    runOpsgenie,
    NetCtx,
    runNetCtx,
  )
where

import Control.Lens ((&), (.~), (?~), (^..), (^?))
import Data.Aeson.Lens (key, values, _String)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS (pack)
import Data.Functor ((<&>))
import Data.Text (Text)
import IO (Env)
import IO qualified as Env (lookup)
import Network.Wreq (asValue, auth, basicAuth, defaults, param, responseBody)
import Network.Wreq.Session (Session, getWith, newAPISession)
import Polysemy (Embed, InterpreterFor, Member, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error, note)
import Polysemy.Input (Input, input, runInputConst)
import Text.Printf (printf)
import Prelude hiding (id, lookup)

data Opsgenie m a where
  WhoIsOnCall :: Text -> Opsgenie m [Text]

makeSem ''Opsgenie

runOpsgenie ::
  ( Member (Embed IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  InterpreterFor Opsgenie r
runOpsgenie = interpret \case
  WhoIsOnCall scheduleID -> getOnCalls scheduleID

data NetCtx = NetCtx
  { apiToken :: ByteString,
    session :: Session
  }

runNetCtx ::
  ( Member (Embed IO) r,
    Member Env r,
    Member (Error Text) r
  ) =>
  InterpreterFor (Input NetCtx) r
runNetCtx program = do
  session <- embed newAPISession
  apiToken <-
    Env.lookup "OPSGENIE_API_TOKEN"
      >>= note "OPSGENIE_API_TOKEN env variable not set"
      <&> BS.pack
  runInputConst (NetCtx {..}) program

getOnCalls ::
  ( Member (Embed IO) r,
    Member (Input NetCtx) r,
    Member (Error Text) r
  ) =>
  Text ->
  Sem r [Text]
getOnCalls scheduleID = do
  netCtx <- input @NetCtx
  let opts =
        defaults
          & auth ?~ basicAuth "" (apiToken netCtx)
          & param "scheduleIdentifierType" .~ ["id"] -- The default value
          & param "flat" .~ ["true"]
  let url = printf "https://api.opsgenie.com/v2/schedules/%s/on-calls" scheduleID

  response <-
    getWith opts (session netCtx) url
      >>= asValue
      & embed
  recipientsArray <-
    response
      & (^? responseBody . key "data" . key "onCallRecipients")
      & note "Expected Opsgenie response to include a \"data.onCallRecipients\" field"
  return (recipientsArray ^.. values . _String)
