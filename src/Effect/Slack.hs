{-# LANGUAGE FlexibleInstances #-}

module Effect.Slack
  ( SlackEffect (..),
  )
where

import Data.Text (Text)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Data.Hashable (Hashable (hashWithSalt))

-- These implementations are not strictly correct for all implementations but
-- are sufficient for our use here.
instance Show ([Text] -> Text) where
  show f = printf "\"%s\"" $ f ["input 1", "input 2", ".."]

instance Hashable ([Text] -> Text) where
  hashWithSalt s f = hashWithSalt s (show f)

instance Eq ([Text] -> Text) where
  a == b = a inputs == b inputs
    where
      inputs = ["input 1", "input 2", ".."]

data SlackEffect
  = SetChannelTopic
      { name :: Text,
        topic :: [Text] -> Text
      }
  | SetGroup
      { handle :: Text,
        name :: Text,
        channels :: [Text]
      }
  deriving (Generic, Show, Eq)

instance FromDhall SlackEffect
instance Hashable SlackEffect
