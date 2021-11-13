{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS (pack)
import Data.List (elem, intercalate, isPrefixOf, null, partition)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Data.Text.IO (getContents, readFile)
import qualified Data.Text.IO as TIO (putStrLn)
import Data.Traversable (traverse)
import Lib (ensure, dryRunEnsure, listUsers)
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
    runDryRunEnsure =<< readInput input
  | not (null flags) = printFlags >> usage >> exitFailure
  | otherwise = runEnsure =<< readInput input
  where
    printFlags = putStrLn ("Unknown flags: " <> intercalate "," flags)
    (flags, input) = partition ("-" `isPrefixOf`) rest
parse args
  | not (null flags) = printFlags >> usage >> exitFailure
  where
    flags = filter ("-" `isPrefixOf`) args
    printFlags = putStrLn ("Unknown flags: " <> intercalate "," flags)
parse ["list-users"] = runListUsers
parse _ = usage >> exitFailure

usage :: IO ()
usage =
  putStrLn
    "Usage: take-care [-h|--help] list-users\n\
    \                             ensure [file ..]\n\
    \                             ensure --dry-run [file ..]\n"

runEnsure :: Text -> IO ()
runEnsure inputText = getApiToken >>= (handleResult . ensure inputText)

runDryRunEnsure :: Text -> IO ()
runDryRunEnsure inputText = getApiToken >>= (handleResult . dryRunEnsure inputText)

runListUsers :: IO ()
runListUsers = getApiToken >>= (handleResult . listUsers)

handleResult :: ExceptT Text IO Text -> IO ()
handleResult result = runExceptT result >>= logResult
  where
    logResult (Left message) = TIO.putStrLn message >> exitFailure
    logResult (Right message) = TIO.putStrLn message >> exitSuccess

getApiToken :: IO ByteString
getApiToken =
  BS.pack . fromMaybe (error "API_TOKEN env variable not set")
    <$> lookupEnv "API_TOKEN"

readInput :: [String] -> IO Text
readInput [] = getContents
readInput fs = mconcat <$> traverse readFile fs
