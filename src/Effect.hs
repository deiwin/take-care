{-# LANGUAGE FlexibleInstances #-}

module Effect
  ( Effect (..),
  )
where

import Dhall (FromDhall)
import Effect.Slack (SlackEffect (..))
import GHC.Generics (Generic)
import Prelude hiding (lines, replicate)

data Effect
  = Slack SlackEffect
  | NoOp -- Here to force a union type
  deriving (Generic, Show, Eq)

instance FromDhall Effect
