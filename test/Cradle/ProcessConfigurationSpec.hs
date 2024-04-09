module Cradle.ProcessConfigurationSpec where

import Cradle
import Test.Hspec

spec :: Spec
spec = describe "ProcessConfiguration" $ do
  describe "showCommand" $ do
    it "shows a human-friendly version of the command" $ do
      showCommand (cmd "foo" & addArgs ["bar"]) `shouldBe` "foo bar"
