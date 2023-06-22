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
      writeBashScript "exe" "touch file"
      run "./exe"
      doesFileExist "file" `shouldReturn` True

    it "throws when executable cannot be found" $ do
      run "./exe"
        `shouldThrow` ( \(e :: IOException) ->
                          ioe_filename e == Just "./exe"
                            && ioe_type e == NoSuchThing
                      )

    it "throws when the executable flag isn't set" $ do
      writeFile "exe" "whatever"
      run "./exe"
        `shouldThrow` ( \(e :: IOException) ->
                          ( ioe_filename e == Just "./exe"
                              && ioe_type e == PermissionDenied
                          )
                      )

    it "throws when the hashbang interpreter cannot be found" $ do
      writeFile "exe" "#!/does/not/exist\nfoo"
      makeExecutable "exe"
      run "./exe"
        `shouldThrow` ( \(e :: IOException) ->
                          ioe_filename e == Just "./exe"
                            && ioe_type e == NoSuchThing
                      )

    it "throws when the exitcode is not 0" $ do
      writeBashScript "exe" "exit 42"
      run "./exe"
        `shouldThrowShow` "command failed with exitcode 42: ./exe"

writeBashScript :: FilePath -> String -> IO ()
writeBashScript file code = do
  bashPath <- getEnv "BASH_PATH"
  writeFile file ("#!" <> bashPath <> "\n" <> code)
  makeExecutable file

makeExecutable :: FilePath -> IO ()
makeExecutable file = do
  permissions <- getPermissions file
  setPermissions file $ setOwnerExecutable True permissions

shouldThrowShow :: IO a -> String -> IO ()
shouldThrowShow action expected = do
  result <-
    (action >> return Nothing)
      `catch` (\(e :: SomeException) -> return $ Just e)
  case result of
    Nothing -> fail $ "action didn't throw: " <> expected
    Just e -> show e `shouldBe` expected
