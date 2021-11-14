module Slack.ChannelSpec (spec) where

import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  it "works" $ do
    True `shouldBe` True
