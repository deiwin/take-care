{-# LANGUAGE DeriveGeneric      #-}

module Effect
  ( Effect (..)
  )
where

import Data.Text (Text)
import Dhall (FromDhall)
import GHC.Generics (Generic)

data SetSlackChannelTopicRecord = SetSlackChannelTopicRecord
  { name :: Text
  , topic :: [Text] -> Text
  }
  deriving(Generic)

instance FromDhall SetSlackChannelTopicRecord

data Effect = SetSlackChannelTopic SetSlackChannelTopicRecord | InviteToSlackChannel Text | SetSlackGroup Text
  deriving(Generic)

instance FromDhall Effect
