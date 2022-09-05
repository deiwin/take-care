module Main where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Functor (($>))
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
    (runWithOutput . dryRunEnsure) =<< readInput input
  | not (null flags) = printFlags >> usage >> exitFailure
  | otherwise = (runWithoutOutput . ensure) =<< readInput input
  where
    printFlags = putStrLn ("Unknown flags: " <> intercalate "," flags)
    (flags, input) = partition ("-" `isPrefixOf`) rest
parse args
  | not (null flags) = printFlags >> usage >> exitFailure
  where
    flags = filter ("-" `isPrefixOf`) args
    printFlags = putStrLn ("Unknown flags: " <> intercalate "," flags)
parse ["list-users"] = runWithOutput listUsers
parse _ = usage >> exitFailure

usage :: IO ()
usage =
  putStrLn
    "Usage: take-care [-h|--help] list-users\n\
    \                             ensure [file ..]\n\
    \                             ensure --dry-run [file ..]\n"

runWithoutOutput :: Sem Lib.CanonicalEffects () -> IO ()
runWithoutOutput =
  runCanonical
    >>> ($>> "Program completed successfully! Exiting.")
    >=> outputResultAndExit

runWithOutput :: Sem Lib.CanonicalEffects Text -> IO ()
runWithOutput = runCanonical >=> outputResultAndExit

outputResultAndExit :: Either Text Text -> IO ()
outputResultAndExit (Left errorMessage) = TIO.putStrLn ("Failed with error: " <> errorMessage) >> exitFailure
outputResultAndExit (Right result) = TIO.putStrLn result >> exitSuccess

readInput :: [String] -> IO Text
readInput [] = getContents
readInput fs = mconcat <$> traverse readFile fs

infixl 4 $>>

($>>) :: (Functor f, Functor g) => f (g a) -> b -> f (g b)
fa $>> b = ($> b) <$> fa
