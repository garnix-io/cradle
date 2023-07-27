{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module OverloadedStringsSpec where

import Cradle
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = do
  around_ inTempDirectory $ do
    it "works with overloaded strings" $ do
      StdoutUntrimmed _ <- run "ls" ("-la", ())
      return () :: IO ()
