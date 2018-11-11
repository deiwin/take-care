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
import Network.Wreq (defaults, param, header, getWith, postWith, asValue, responseBody, auth
  , oauth2Bearer, Options, Response)
import Data.Maybe (maybeToList, fromMaybe)
import Data.Aeson (FromJSON, encode, toJSON, pairs, (.=), object, Value)
import Control.Lens ((&), (.~), (^?), (^?!), (^..), (^.), (?~))
import Data.Aeson.Lens (key, values, _String, _Bool)
import Data.List (find)
import Data.Map as Map (fromList)

import Debug.Trace (trace)

type Token = ByteString
data InputRecord = InputRecord { members :: [Text]
                               , team :: Text -- TODO should be max 21 chars with the tm- prefix, so 18
                               , topic :: Text -> Text
                               } deriving (Generic, Show)

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

findOrCreateChannelID :: Text -> Token -> IO (Either Text Text)
findOrCreateChannelID name token = do
    currentIDResult <- findChannelID name token
    case currentIDResult of
      Left err -> return $ Left err
      Right currentID -> maybe (createChannelID name token) (return . Right) currentID

createChannelID :: Text -> Token -> IO (Either Text Text)
createChannelID name apiToken = do
    let opts = defaults & auth ?~ oauth2Bearer apiToken
    let body = toJSON $ object ["name" .= name]
    resp <- asValue =<< postWith opts (slackURL "conversations.create") body
    -- TODO Should handle errors safely
    let Just id = trace (show $ resp ^? responseBody) $ resp ^? responseBody . key "channel" . key "id" . _String
    return $ Right id

slackURL :: String -> String
slackURL = ("https://slack.com/api/" ++)

slackGet :: Token -> Options -> String -> IO (Either Text Value)
slackGet apiToken opts method = do
    let optsWithAuth = opts & auth ?~ oauth2Bearer apiToken
    let url = slackURL method
    resp <- asValue =<< getWith optsWithAuth url
    let respBody = resp ^. responseBody
    let ok = respBody ^?! key "ok" . _Bool
    let errorMessage = respBody ^. key "error" . _String
    let errorDetail = respBody ^? key "detail" . _String
    if ok then
        return $ Right respBody
    else
        return $ Left (errorMessage <> fromMaybe ""  errorDetail)


findChannelID :: Text -> Token -> IO (Either Text (Maybe Text))
findChannelID = findChannelID' Nothing
findChannelID' :: Maybe Text -> Text -> Token -> IO (Either Text (Maybe Text))
findChannelID' cursor name apiToken = do
    let opts = defaults & param "cursor" .~ maybeToList cursor
    resp <- slackGet apiToken opts "conversations.list"
    case resp of
      Left err -> return $ Left err
      Right respBody -> do
        let
            channelID = (^.. values) <$> respBody ^? key "channels" >>=
                find (\x -> (x ^? key "name". _String) == Just name) >>=
                    (^? key "id" . _String)
        let
            nextCursor = mfilter (not . T.null) $
                respBody ^? key "response_metadata" . key "next_cursor" . _String
        case channelID of
          Just id -> return $ Right $ Just id
          Nothing -> case nextCursor of
                       Just c -> findChannelID' (Just c) name apiToken
                       Nothing -> return $ Right Nothing
