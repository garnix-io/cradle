module CradleSpec where

import Cradle
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

makeExecutable :: FilePath -> IO ()
makeExecutable file = do
  permissions <- getPermissions file
  setPermissions file $ setOwnerExecutable True permissions
