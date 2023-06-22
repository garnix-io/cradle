{-# LANGUAGE ScopedTypeVariables #-}

module CradleSpec where

import Control.Exception
import Cradle
import GHC.IO.Exception
import System.Directory
import System.Environment
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = do
  around_ inTempDirectory $ do
    it "runs simple commands" $ do
      bashPath <- getEnv "BASH_PATH"
      writeFile "exe" ("#!" <> bashPath <> "\ntouch ./file")
      makeExecutable "exe"
      run "./exe"
      doesFileExist "file" `shouldReturn` True

    it "throws when executable cannot be found" $ do
      run "./exe"
        `shouldThrow` ( \(e :: IOException) ->
                          ioe_filename e == Just "./exe"
                            && ioe_type e == NoSuchThing
                      )

makeExecutable :: FilePath -> IO ()
makeExecutable file = do
  permissions <- getPermissions file
  setPermissions file $ setOwnerExecutable True permissions
