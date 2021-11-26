module Main where

import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS (pack)
import Data.List (intercalate, isPrefixOf, partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO (getContents, readFile)
import qualified Data.Text.IO as TIO (putStrLn)
import Lib (CanonicalEffects, dryRunEnsure, ensure, listUsers, runCanonical)
import Polysemy (Sem)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import Prelude hiding (getContents, readFile)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse args
  | elem "-h" flags
      || elem "--help" flags =
    usage >> exitSuccess
  where
    flags = filter ("-" `isPrefixOf`) args
parse ("ensure" : rest)
  | elem "-D" flags
      || elem "--dry-run" flags =
    (run . dryRunEnsure) =<< readInput input
  | not (null flags) = printFlags >> usage >> exitFailure
  | otherwise = (run . ensure) =<< readInput input
  where
    printFlags = putStrLn ("Unknown flags: " <> intercalate "," flags)
    (flags, input) = partition ("-" `isPrefixOf`) rest
parse args
  | not (null flags) = printFlags >> usage >> exitFailure
  where
    flags = filter ("-" `isPrefixOf`) args
    printFlags = putStrLn ("Unknown flags: " <> intercalate "," flags)
parse ["list-users"] = run listUsers
parse _ = usage >> exitFailure

usage :: IO ()
usage =
  putStrLn
    "Usage: take-care [-h|--help] list-users\n\
    \                             ensure [file ..]\n\
    \                             ensure --dry-run [file ..]\n"

run :: ExceptT Text (Sem Lib.CanonicalEffects) Text -> IO ()
run f = getApiToken >>= flip runCanonical f >>= logResult

logResult :: Either Text Text -> IO ()
logResult (Left message) = TIO.putStrLn message >> exitFailure
logResult (Right message) = TIO.putStrLn message >> exitSuccess

getApiToken :: IO ByteString
getApiToken =
  BS.pack . fromMaybe (error "API_TOKEN env variable not set")
    <$> lookupEnv "API_TOKEN"

readInput :: [String] -> IO Text
readInput [] = getContents
readInput fs = mconcat <$> traverse readFile fs
