{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Lib
    ( someFunc
    ) where

import Dhall (input, auto, Interpret)
import Data.Text as T (Text, pack, null)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Text.Show.Functions ()
import System.FilePath (FilePath)
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Control.Monad ((>=>), mfilter)
import Network.Wreq (defaults, param, header, getWith, postWith, asValue, responseBody, auth, oauth2Bearer)
import Data.Maybe (maybeToList)
import Data.Aeson (FromJSON, encode, toJSON, pairs, (.=), object)
import Control.Lens ((&), (.~), (^?), (^..), (^.), (?~))
import Data.Aeson.Lens (key, values, _String)
import Data.List (find)
import Data.Map as Map (fromList)

import Debug.Trace (trace)

type Token = ByteString
data InputRecord = InputRecord { members :: [Text]
                               , team :: Text -- TODO should be max 21 chars with the tm- prefix, so 18
                               , topic :: Text -> Text
                               } deriving (Generic, Show)

u :: String -> String
u = ("https://slack.com/api/" ++)

instance Interpret InputRecord

readInput :: FilePath -> IO [InputRecord]
readInput = input auto . T.pack

someFunc :: FilePath -> Token -> IO ()
someFunc inputPath apiToken = do
    input <- readInput inputPath
    traverse_ print input
    -- channelID <- findChannelID ("tm-" ++ name $ fst input)
    channelID <- findOrCreateChannelID "akahsa-testx" apiToken
    print channelID

findOrCreateChannelID :: Text -> Token -> IO Text
findOrCreateChannelID name token = do
    currentID <- findChannelID name token
    maybe (createChannelID name token) return currentID

createChannelID :: Text -> Token -> IO Text
createChannelID name apiToken = do
    let opts = defaults & auth ?~ oauth2Bearer apiToken
    let body = toJSON $ object ["name" .= name]
    resp <- asValue =<< postWith opts (u "conversations.create") body
    -- TODO Should handle errors safely
    let Just id = trace (show $ resp ^? responseBody) $ resp ^? responseBody . key "channel" . key "id" . _String
    return id

findChannelID :: Text -> Token -> IO (Maybe Text)
findChannelID = findChannelID' Nothing
findChannelID' :: Maybe Text -> Text -> Token -> IO (Maybe Text)
findChannelID' cursor name apiToken = do
    let opts = defaults & auth ?~ oauth2Bearer apiToken
                        & param "cursor" .~ maybeToList cursor
    resp <- asValue =<< getWith opts (u "conversations.list")
    let
        channelID = (^.. values) <$> resp ^? responseBody . key "channels" >>=
            find (\x -> (x ^? key "name". _String) == Just name) >>=
                (^? key "id" . _String)
    let
        nextCursor = mfilter (not . T.null) $
            resp ^? responseBody . key "response_metadata" . key "next_cursor" . _String
    case channelID of
      Just id -> return $ Just id
      Nothing -> case nextCursor of
                   Just c -> findChannelID' (Just c) name apiToken
                   Nothing -> return Nothing
