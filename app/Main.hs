{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (ensure)
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
parse ("ensure":rest) = runEnsure =<<
    case rest of
      [] -> getContents
      fs -> mconcat <$> traverse readFile fs
parse _ = usage >> exitFailure

usage :: IO ()
usage = putStrLn "Usage: take-care ensure [-h|--help] [file ..]"

runEnsure :: Text -> IO ()
runEnsure inputText = getApiToken >>= ensure inputText

getApiToken :: IO ByteString
getApiToken = BS.pack . fromMaybe (error "API_TOKEN env variable not set") <$>
        lookupEnv "API_TOKEN"
