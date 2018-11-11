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
    let body = toJSON $ object ["name" .= name]
    resp <- slackPost apiToken defaults "conversations.create" body
    case resp of
      Left err -> return $ Left err
      Right respBody -> do
        let Just id = trace (show respBody) $ respBody ^? key "channel" . key "id" . _String
        return $ Right id

slackURL :: String -> String
slackURL = ("https://slack.com/api/" ++)

slackGet :: Token -> Options -> String -> IO (Either Text Value)
slackGet apiToken opts method = handleSlackError "GET" method <$> (asValue =<< getWith optsWithAuth url)
    where optsWithAuth = opts & auth ?~ oauth2Bearer apiToken
          url = slackURL method

slackPost :: Token -> Options -> String -> Value -> IO (Either Text Value)
slackPost apiToken opts method body = handleSlackError "POST" method <$> (asValue =<< postWith optsWithAuth url body)
    where optsWithAuth = opts & auth ?~ oauth2Bearer apiToken
          url = slackURL method

handleSlackError :: Text -> String -> Response Value -> Either Text Value
handleSlackError httpMethod method resp =
    let respBody = resp ^. responseBody
        ok = respBody ^?! key "ok" . _Bool
        error = respBody ^. key "error" . _String
        detail = respBody ^? key "detail" . _String
     in if ok then
            Right respBody
        else
            Left (httpMethod <> " " <> T.pack method <> ": " <> error <> maybe "" (" - " <>) detail)


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
