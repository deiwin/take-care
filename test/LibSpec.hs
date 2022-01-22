module LibSpec (spec) where

import Data.Function ((&))
import Data.Text (Text)
import Lib (listUsers)
import Polysemy (Sem, interpret, run)
import Slack.User (User (..), Users (..))
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "listUsers" $ do
    it "returns an empty string for an empty list of users" $ do
      listUsers
        & runConstListUsers []
        & (`shouldBe` "")

    it "pretty prints the user IDs and display names" $ do
      listUsers
        & runConstListUsers
          [ User {_id = "id1", _displayName = "@name1"},
            User {_id = "id2", _displayName = "@name2"}
          ]
        & ( `shouldBe`
              "id1: @name1\n\
              \id2: @name2\n"
          )

runConstListUsers :: [User] -> Sem '[Users] Text -> Text
runConstListUsers users =
  run . interpret \case
    ListAll -> return users
