{-# LANGUAGE ScopedTypeVariables #-}

module CradleSpec where

import Control.Exception
import Control.Monad.Trans.Identity
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
      run_ "./exe"
      doesFileExist "file" `shouldReturn` True

    it "throws when executable cannot be found" $ do
      run_ "./exe"
        `shouldThrow` ( \(e :: IOException) ->
                          ioe_filename e == Just "./exe"
                            && ioe_type e == NoSuchThing
                      )

    it "throws when the executable flag isn't set" $ do
      writeFile "exe" "whatever"
      run_ "./exe"
        `shouldThrow` ( \(e :: IOException) ->
                          ( ioe_filename e == Just "./exe"
                              && ioe_type e == PermissionDenied
                          )
                      )

    it "throws when the hashbang interpreter cannot be found" $ do
      writeFile "exe" "#!/does/not/exist\nfoo"
      makeExecutable "exe"
      run_ "./exe"
        `shouldThrow` ( \(e :: IOException) ->
                          ioe_filename e == Just "./exe"
                            && ioe_type e == NoSuchThing
                      )

    it "throws when the exitcode is not 0" $ do
      writeBashScript "exe" "exit 42"
      run_ "./exe"
        `shouldThrowShow` "command failed with exitcode 42: ./exe"

    it "allows to be run in MonadIO contexts" $ do
      writeBashScript "exe" "true"
      runIdentityT $ do
        _ :: String <- run "./exe"
        run_ "./exe"

    it "allows to pass in an argument" $ do
      writeBashScript "exe" "echo $1 > file"
      run_ ("./exe", "foo")
      readFile "file" `shouldReturn` "foo\n"

    it "allows to pass in multiple arguments" $ do
      writeBashScript "exe" "echo $1 > file; echo $2 >> file"
      run_ ("./exe", ["foo", "bar"])
      readFile "file" `shouldReturn` "foo\nbar\n"

    it "provides nice syntax for multiple arguments" $ do
      writeBashScript "exe" "echo $1 > file"
      run_ "./exe" "foo"
      readFile "file" `shouldReturn` "foo\n"
      writeBashScript "exe" "echo $1 > file; echo $2 >> file"
      run_ "./exe" "foo" "bar"
      readFile "file" `shouldReturn` "foo\nbar\n"

    it "allows to capture stdout" $ do
      writeBashScript "exe" "echo output"
      stdout <- run "./exe"
      stdout `shouldBe` "output\n"

    it "allows to capture stdout *and* pass in arguments" $ do
      writeBashScript "exe" "echo $1"
      output <- run "./exe" "foo"
      output `shouldBe` "foo\n"
      writeBashScript "exe" "echo $1; echo $2"
      output <- run "./exe" "foo" "bar"
      output `shouldBe` "foo\nbar\n"

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
