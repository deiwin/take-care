{-# LANGUAGE FlexibleInstances #-}

module Effect
  ( Effect (..),
  )
where

import Data.Text (Text)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Text.Printf (printf)

-- These implementations are not strictly correct for all implementations but
-- are sufficient for our use here.
instance Show ([Text] -> Text) where
  show f = printf "\"%s\"" $ f ["input 1", "input 2", ".."]

instance Eq ([Text] -> Text) where
  a == b = a inputs == b inputs
    where
      inputs = ["input 1", "input 2", ".."]

data Effect
  = SlackSetChannelTopic
      { name :: Text,
        topic :: [Text] -> Text
      }
  | SlackSetGroup
      { handle :: Text,
        name :: Text,
        channels :: [Text]
      }
  deriving (Generic, Show, Eq)

instance FromDhall Effect
