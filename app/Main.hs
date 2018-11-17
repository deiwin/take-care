{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (ensure)
import System.Environment (lookupEnv)
import Data.ByteString.Char8 as BS (pack)
import Prelude hiding (getContents)
import Data.Text.IO (getContents)

main :: IO ()
main = do
    inputText <- getContents
    Just apiToken <- lookupEnv "API_TOKEN"
    ensure inputText (BS.pack apiToken)
