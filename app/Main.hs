{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (ensure)
import System.Environment (lookupEnv)
import Data.ByteString.Char8 as BS (pack)
import Prelude hiding (getContents)
import Data.Text.IO (getContents)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    inputText <- getContents
    apiToken <- fromMaybe (error "API_TOKEN env variable not set") <$> lookupEnv "API_TOKEN"
    ensure inputText (BS.pack apiToken)
