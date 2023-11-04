{-# LANGUAGE ScopedTypeVariables #-}

module CradleSpec where

import Control.Exception
import Control.Monad.Trans.Identity
import Cradle
import Data.String.Conversions
import Data.Text (Text)
import GHC.IO.Exception
import System.Directory
import System.Environment
import System.IO (hGetContents, stderr)
import System.IO.Silently
import System.Process (createPipe)
import Test.Hspec
import Test.Mockery.Directory
import Prelude hiding (getContents)

spec :: Spec
spec = do
  around_ inTempDirectory $ do
    it "runs simple commands" $ do
      writePythonScript "exe" "open('file', 'w').write('')"
      run_ "./exe"
      doesFileExist "file" `shouldReturn` True

    describe "exceptions" $ do
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

    it "allows to be run in MonadIO contexts" $ do
      writePythonScript "exe" "pass"
      runIdentityT $ do
        _ :: StdoutUntrimmed <- run "./exe"
        run_ "./exe"

    describe "arguments" $ do
      let writeArgumentWriter = do
            writePythonScript "exe" $
              "open('file', 'w').write(' '.join(sys.argv[1:]))"

      it "allows to pass in an argument" $ do
        writeArgumentWriter
        run_ ("./exe", "foo")
        readFile "file" `shouldReturn` "foo"

      it "allows to pass in multiple arguments" $ do
        writeArgumentWriter
        run_ ("./exe", ["foo", "bar"])
        readFile "file" `shouldReturn` "foo bar"

      it "provides nice syntax for multiple arguments" $ do
        writeArgumentWriter
        run_ "./exe" "foo"
        readFile "file" `shouldReturn` "foo"
        run_ "./exe" "foo" "bar"
        readFile "file" `shouldReturn` "foo bar"

      it "allows to split strings in haskell" $ do
        StdoutTrimmed output <- run $ words "echo foo"
        output `shouldBe` cs "foo"

      it "allows Text as arguments" $ do
        StdoutTrimmed output <- run "echo" (cs "foo" :: Text)
        output `shouldBe` cs "foo"

    describe "capturing stdout" $ do
      it "allows to capture stdout" $ do
        writePythonScript "exe" "print('output')"
        StdoutUntrimmed stdout <- run "./exe"
        stdout `shouldBe` cs "output\n"

      it "allows to capture stdout *and* pass in arguments" $ do
        writePythonScript "exe" "print(' '.join(sys.argv[1:]))"
        StdoutUntrimmed output <- run "./exe" "foo"
        output `shouldBe` cs "foo\n"
        StdoutUntrimmed output <- run "./exe" "foo" "bar"
        output `shouldBe` cs "foo bar\n"

      it "relays stdout when it's not captured (by default)" $ do
        writePythonScript "exe" "print('foo')"
        output <- capture_ $ run_ "./exe"
        output `shouldBe` "foo\n"

      it "does not relay stdout when it's captured (by default)" $ do
        writePythonScript "exe" "print('foo')"
        output <- capture_ $ do
          StdoutUntrimmed _ <- run "./exe"
          return ()
        output `shouldBe` ""

      describe "StdoutTrimmed" $ do
        it "allows to capture the stripped stdout" $ do
          writePythonScript "exe" "print('foo')"
          StdoutTrimmed output <- run "./exe" "foo"
          output `shouldBe` cs "foo"

        it "strips leading and trailing spaces and newlines" $ do
          writePythonScript "exe" "print('  foo   ')"
          StdoutTrimmed output <- run "./exe" "foo"
          output `shouldBe` cs "foo"

    describe "capture stderr" $ do
      it "allows to capture stderr" $ do
        writePythonScript "exe" "print('output', file=sys.stderr)"
        Stderr stderr <- run "./exe"
        stderr `shouldBe` cs "output\n"

      it "relays stderr when it's not captured (by default)" $ do
        writePythonScript "exe" "print('foo', file=sys.stderr)"
        output <- hCapture_ [stderr] $ run_ "./exe"
        output `shouldBe` "foo\n"

      it "does not relay stderr when it's captured (by default)" $ do
        writePythonScript "exe" "print('foo', file=sys.stderr)"
        output <- hCapture_ [stderr] $ do
          Stderr _ <- run "./exe"
          return ()
        output `shouldBe` ""

      describe "using handles" $ do
        it "allows to send stderr to a handle" $ do
          writePythonScript "exe" "print('foo', file=sys.stderr)"
          (readEnd, writeEnd) <- createPipe
          run_ "./exe" (StderrHandle writeEnd)
          hGetContents readEnd `shouldReturn` cs "foo\n"

        it "does not relay stderr when it's captured (by default)" $ do
          writePythonScript "exe" "print('foo', file=sys.stderr)"
          (_readEnd, writeEnd) <- createPipe
          stderr <- hCapture_ [stderr] $ run_ "./exe" (StderrHandle writeEnd)
          stderr `shouldBe` ""

    it "allows to capture both stdout and stderr" $ do
      writePythonScript "exe" "print('out') ; print('err', file=sys.stderr)"
      (StdoutUntrimmed out, Stderr err) <- run "./exe"
      (out, err) `shouldBe` (cs "out\n", cs "err\n")

    describe "exitcodes" $ do
      it "throws when the exitcode is not 0" $ do
        writePythonScript "exe" "sys.exit(42)"
        run_ "./exe"
          `shouldThrowShow` "command failed with exitcode 42: ./exe"

      it "doesn't throw when the exitcode is captured" $ do
        writePythonScript "exe" "sys.exit(42)"
        exitCode <- run "./exe"
        exitCode `shouldBe` ExitFailure 42

      it "captures success exitcodes" $ do
        writePythonScript "exe" "pass"
        run "./exe" `shouldReturn` ExitSuccess

      it "allows to capture exitcodes and stdout" $ do
        writePythonScript "exe" "print('foo'); sys.exit(42)"
        (exitCode, StdoutTrimmed stdout) <- run "./exe"
        stdout `shouldBe` cs "foo"
        exitCode `shouldBe` ExitFailure 42

writePythonScript :: FilePath -> String -> IO ()
writePythonScript file code = do
  pythonPath <- getEnv "PYTHON_BIN_PATH"
  writeFile file ("#!" <> pythonPath <> "\nimport sys\n" <> code)
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
