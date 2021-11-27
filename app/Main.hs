module Main where

import Control.Monad ((>=>))
import Data.List (intercalate, isPrefixOf, partition)
import Data.Text (Text)
import Data.Text.IO (getContents, readFile)
import qualified Data.Text.IO as TIO (putStrLn)
import Lib (CanonicalEffects, dryRunEnsure, ensure, listUsers, runCanonical)
import Polysemy (Sem)
import System.Environment (getArgs)
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

run :: Sem Lib.CanonicalEffects Text -> IO ()
run = runCanonical >=> logResult

logResult :: Either Text Text -> IO ()
logResult (Left message) = TIO.putStrLn message >> exitFailure
logResult (Right message) = TIO.putStrLn message >> exitSuccess

readInput :: [String] -> IO Text
readInput [] = getContents
readInput fs = mconcat <$> traverse readFile fs
