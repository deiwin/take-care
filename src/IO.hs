module IO
  ( -- * Env effect for env variables
    Env,
    runEnv,
    lookup,

    -- * Time effect
    Time,
    runTime,
    getCurrent,
  )
where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Polysemy (Embed, InterpreterFor, Member, embed, interpret, makeSem)
import System.Environment (lookupEnv)
import Prelude hiding (lookup)

data Env m a where
  Lookup :: String -> Env m (Maybe String)

makeSem ''Env

runEnv :: Member (Embed IO) r => InterpreterFor Env r
runEnv = interpret \case
  Lookup name -> embed $ lookupEnv name

data Time m a where
  GetCurrent :: Time m UTCTime

makeSem ''Time

runTime :: Member (Embed IO) r => InterpreterFor Time r
runTime = interpret \case
  GetCurrent -> embed getCurrentTime
