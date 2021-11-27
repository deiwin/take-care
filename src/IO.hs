module IO
  ( lookup,
    runEnv,
    Env,
  )
where

import Polysemy (Embed, InterpreterFor, Member, embed, interpret, makeSem)
import System.Environment (lookupEnv)
import Prelude hiding (lookup)

data Env m a where
  Lookup :: String -> Env m (Maybe String)

makeSem ''Env

runEnv :: Member (Embed IO) r => InterpreterFor Env r
runEnv = interpret \case
  Lookup name -> embed $ lookupEnv name
