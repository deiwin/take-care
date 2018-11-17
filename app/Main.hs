{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (ensure, listCaretakers, listUsers)
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS (pack)
import Prelude hiding (getContents, readFile)
import Data.Text (Text)
import Data.Text.IO (getContents, readFile)
import Data.List (elem, isPrefixOf, null)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Monoid (mconcat)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse args
  |  elem "-h" flags
  || elem "--help" flags = usage >> exitSuccess
  | not (null flags) = usage >> exitFailure
  where flags = filter ("-" `isPrefixOf`) args
parse ("ensure":rest) = runEnsure =<< readInput rest
parse ("list-caretakers":rest) = runListCaretakers =<< readInput rest
parse ["list-users"] = runListUsers
parse _ = usage >> exitFailure

usage :: IO ()
usage = putStrLn "Usage: take-care [-h|--help] ensure [file ..]\n\
                 \                             list-caretakers [file ..]\n\
                 \                             list-users\n"

runEnsure :: Text -> IO ()
runEnsure inputText = getApiToken >>= ensure inputText
runListCaretakers :: Text -> IO ()
runListCaretakers inputText = getApiToken >>= listCaretakers inputText
runListUsers :: IO ()
runListUsers = getApiToken >>= listUsers

getApiToken :: IO ByteString
getApiToken = BS.pack . fromMaybe (error "API_TOKEN env variable not set") <$>
        lookupEnv "API_TOKEN"

readInput :: [String] -> IO Text
readInput [] = getContents
readInput fs = mconcat <$> traverse readFile fs
