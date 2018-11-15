{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (ensure)
import System.Environment (lookupEnv)
import Data.ByteString.Char8 as BS (pack)

main :: IO ()
main = do
    let inputPath = "./teams.dhall"
    Just apiToken <- lookupEnv "API_TOKEN"
    ensure inputPath (BS.pack apiToken)
