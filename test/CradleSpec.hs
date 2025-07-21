{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module CradleSpec where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception
import Control.Monad (void, when)
import Control.Monad.Trans.Identity
import Cradle
import Data.ByteString (pack)
import Data.String.Conversions
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text (Text)
import qualified Data.Text
import GHC.IO.Exception
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hClose, hGetContents, hIsClosed, hPutStrLn, stderr, stdout)
import System.IO.Silently
import System.Process (createPipe)
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment (withModifiedEnvironment)
import Prelude hiding (getContents, length)

spec :: Spec
spec = do
  around_ inTempDirectory $ do
    it "runs simple commands" $ do
      writePythonScript "exe" "open('file', 'w').write('')"
      run_ $ cmd "./exe"
      doesFileExist "file" `shouldReturn` True

    describe "exceptions" $ do
      it "throws when executable cannot be found" $ do
        run_ (cmd "./exe")
          `shouldThrow` ( \(e :: IOException) ->
                            ioe_filename e == Just "./exe"
                              && ioe_type e == NoSuchThing
                        )

      it "throws when the executable flag isn't set" $ do
        writeFile "exe" "whatever"
        run_ (cmd "./exe")
          `shouldThrow` ( \(e :: IOException) ->
                            ioe_filename e == Just "./exe"
                              && ioe_type e == PermissionDenied
                        )

      it "throws when the hashbang interpreter cannot be found" $ do
        writeFile "exe" "#!/does/not/exist\nfoo"
        makeExecutable "exe"
        run_ (cmd "./exe")
          `shouldThrow` ( \(e :: IOException) ->
                            ioe_filename e == Just "./exe"
                              && ioe_type e == NoSuchThing
                        )

    it "runs in MonadIO contexts" $ do
      writePythonScript "exe" "pass"
      runIdentityT $ do
        _ :: StdoutUntrimmed <- run $ cmd "./exe"
        run_ $ cmd "./exe"

    describe "arguments" $ do
      let writeArgumentWriter = do
            writePythonScript
              "exe"
              "open('file', 'w').write(' '.join(sys.argv[1:]))"

      it "allows passing in an argument" $ do
        writeArgumentWriter
        run_ $
          cmd "./exe"
            & addArgs ["foo"]
        readFile "file" `shouldReturn` "foo"

      it "allows passing in multiple arguments" $ do
        writeArgumentWriter
        run_ $
          cmd "./exe"
            & addArgs ["foo", "bar"]
        readFile "file" `shouldReturn` "foo bar"

      it "allows splitting strings in haskell" $ do
        StdoutUntrimmed output <-
          run $
            cmd "printf"
              & addArgs (words "%s.%s foo bar")
        output `shouldBe` cs "foo.bar"

      it "allows Text as arguments" $ do
        StdoutTrimmed output <-
          run $
            cmd "echo"
              & addArgs [cs "foo" :: Text]
        output `shouldBe` cs "foo"

    describe "providing stdin" $ do
      describe "using handles" $ do
        it "allows passing in a handle for stdin" $ do
          writePythonScript "exe" "print(sys.stdin.read().strip())"
          (readEnd, writeEnd) <- createPipe
          _ <- forkIO $ do
            hPutStrLn writeEnd "test stdin"
            hClose writeEnd
          StdoutUntrimmed output <-
            run $
              cmd "./exe"
                & setStdinHandle readEnd
          output `shouldBe` cs "test stdin\n"

      it "allows disabling inheriting stdin" $ do
        writePythonScript "exe" "print(sys.stdin)"
        StdoutTrimmed output <- run $ cmd "./exe" & setNoStdin
        output `shouldBe` cs "None"

    describe "capturing stdout" $ do
      it "allows capturing stdout" $ do
        writePythonScript "exe" "print('output')"
        StdoutUntrimmed stdout <- run $ cmd "./exe"
        stdout `shouldBe` cs "output\n"

      it "allows capturing stdout *and* pass in arguments" $ do
        writePythonScript "exe" "print(' '.join(sys.argv[1:]))"
        StdoutUntrimmed output <-
          run $
            cmd "./exe"
              & addArgs ["foo"]
        output `shouldBe` cs "foo\n"
        StdoutUntrimmed output <- run $ cmd "./exe" & addArgs ["foo", "bar"]
        output `shouldBe` cs "foo bar\n"

      it "relays stdout when it's not captured (by default)" $ do
        writePythonScript "exe" "print('foo')"
        output <- capture_ $ run_ $ cmd "./exe"
        output `shouldBe` "foo\n"

      it "allows silencing stdout when it's not captured" $ do
        writePythonScript "exe" "print('foo')"
        output <- hCapture_ [stdout] $ run_ $ cmd "./exe" & silenceStdout
        output `shouldBe` ""

      it "does not relay stdout when it's captured (by default)" $ do
        writePythonScript "exe" "print('foo')"
        output <- capture_ $ do
          StdoutUntrimmed _ <- run $ cmd "./exe"
          return ()
        output `shouldBe` ""

      describe "StdoutTrimmed" $ do
        it "allows capturing the stripped stdout" $ do
          writePythonScript "exe" "print('foo')"
          StdoutTrimmed output <- run $ cmd "./exe" & addArgs ["foo"]
          output `shouldBe` cs "foo"

        it "strips leading and trailing spaces and newlines" $ do
          writePythonScript "exe" "print('  foo   ')"
          StdoutTrimmed output <- run $ cmd "./exe" & addArgs ["foo"]
          output `shouldBe` cs "foo"

      it "handles bigger outputs correctly" $ do
        writePythonScript "exe" "print('x' * 2 ** 22)"
        StdoutTrimmed output <- run $ cmd "./exe"
        Data.Text.length output `shouldBe` 2 ^ (22 :: Int)

      it "allows capturing binary data" $ do
        writePythonScript "exe" "sys.stdout.buffer.write(bytearray([0, 1, 2, 3]))"
        StdoutRaw output <- run $ cmd "./exe"
        output `shouldBe` pack [0, 1, 2, 3]

      it "uses the replacement character when capturing invalid utf-8 as Text" $ do
        writePythonScript "exe" "sys.stdout.buffer.write(b'foo-\\x80-bar')"
        StdoutUntrimmed output <- run $ cmd "./exe"
        output `shouldBe` cs "foo-�-bar"

      describe "addModifier" $ do
        it "allows running modifiers with IO actions" $ do
          let config =
                cmd "echo"
                  & addModifier
                    ( do
                        StdoutTrimmed output <- run $ cmd "echo" & addArgs ["hi"]
                        return $ addArgs [output]
                    )
          StdoutTrimmed output <- run config
          cs output `shouldBe` "hi"

      describe "using handles" $ do
        it "allows sending stdout to a handle" $ do
          writePythonScript "exe" "print('foo')"
          (readEnd, writeEnd) <- createPipe
          run_ $ cmd "./exe" & addStdoutHandle writeEnd
          hClose writeEnd
          hGetContents readEnd `shouldReturn` cs "foo\n"

        it "does not relay stdout when it's captured (by default)" $ do
          writePythonScript "exe" "print('foo')"
          (_readEnd, writeEnd) <- createPipe
          stdout <- capture_ $ run_ $ cmd "./exe" & addStdoutHandle writeEnd
          hClose writeEnd
          stdout `shouldBe` ""

        it "doesn't close the handle after running the process" $ do
          writePythonScript "exe" "print(sys.argv[1])"
          (readEnd, writeEnd) <- createPipe
          run_ $
            cmd "./exe"
              & addArgs ["foo"]
              & addStdoutHandle writeEnd
          hIsClosed writeEnd `shouldReturn` False
          hIsClosed readEnd `shouldReturn` False
          run_ $
            cmd "./exe"
              & addArgs ["bar"]
              & addStdoutHandle writeEnd
          hClose writeEnd
          hGetContents readEnd `shouldReturn` cs "foo\nbar\n"

        it "allows capturing stdout *and* passing in a stdout handle" $ do
          writePythonScript "exe" "print('foo')"
          (readEnd, writeEnd) <- createPipe
          StdoutUntrimmed output <-
            run $
              cmd "./exe"
                & addStdoutHandle writeEnd
          hClose writeEnd
          handleOutput <- hGetContents readEnd
          (output, handleOutput) `shouldBe` (cs "foo\n", "foo\n")

        it "allows specifying multiple stdout handles" $ do
          writePythonScript "exe" "print('foo')"
          (readEnd1, writeEnd1) <- createPipe
          (readEnd2, writeEnd2) <- createPipe
          run_ $
            cmd "./exe"
              & addStdoutHandle writeEnd1
              & addStdoutHandle writeEnd2
          hClose writeEnd1
          hClose writeEnd2
          output1 <- hGetContents readEnd1
          output2 <- hGetContents readEnd2
          (output1, output2) `shouldBe` ("foo\n", "foo\n")

    describe "capture stderr" $ do
      it "allows capturing stderr" $ do
        writePythonScript "exe" "print('output', file=sys.stderr)"
        StderrRaw stderr <- run $ cmd "./exe"
        stderr `shouldBe` cs "output\n"

      it "relays stderr when it's not captured (by default)" $ do
        writePythonScript "exe" "print('foo', file=sys.stderr)"
        output <- hCapture_ [stderr] $ run_ $ cmd "./exe"
        output `shouldBe` "foo\n"

      it "allows silencing stderr when it's not captured" $ do
        writePythonScript "exe" "print('foo', file=sys.stderr)"
        output <- hCapture_ [stderr] $ run_ $ cmd "./exe" & silenceStderr
        output `shouldBe` ""

      it "does not relay stderr when it's captured (by default)" $ do
        writePythonScript "exe" "print('foo', file=sys.stderr)"
        output <- hCapture_ [stderr] $ do
          StderrRaw _ <- run $ cmd "./exe"
          return ()
        output `shouldBe` ""

      describe "using handles" $ do
        it "allows sending stderr to a handle" $ do
          writePythonScript "exe" "print('foo', file=sys.stderr)"
          (readEnd, writeEnd) <- createPipe
          run_ $
            cmd "./exe"
              & addStderrHandle writeEnd
          hClose writeEnd
          hGetContents readEnd `shouldReturn` cs "foo\n"

        it "does not relay stderr when it's captured (by default)" $ do
          writePythonScript "exe" "print('foo', file=sys.stderr)"
          (_readEnd, writeEnd) <- createPipe
          stderr <- hCapture_ [stderr] $ run_ $ cmd "./exe" & addStderrHandle writeEnd
          hClose writeEnd
          stderr `shouldBe` ""

        it "doesn't close the handle after running the process" $ do
          writePythonScript "exe" "print(sys.argv[1], file=sys.stderr)"
          (readEnd, writeEnd) <- createPipe
          run_ $
            cmd "./exe"
              & addStderrHandle writeEnd
              & addArgs ["foo"]
          hIsClosed writeEnd `shouldReturn` False
          hIsClosed readEnd `shouldReturn` False
          run_ $
            cmd "./exe"
              & addStderrHandle writeEnd
              & addArgs ["bar"]
          hClose writeEnd
          hGetContents readEnd `shouldReturn` cs "foo\nbar\n"

        it "allows writing a wrapper that captures stderr" $ do
          let wrapper :: Output output => ProcessConfiguration -> IO output
              wrapper pc = do
                (exitCode, StderrRaw err, o) <- run pc
                when (exitCode /= ExitSuccess) $ do
                  throwIO $ ErrorCall $ "stderr:\n" <> cs err
                return o
          writePythonScript "exe" "print('foo', file=sys.stderr)"
          (readEnd, writeEnd) <- createPipe
          StderrRaw output <-
            wrapper $
              cmd "./exe"
                & addStderrHandle writeEnd
          hClose writeEnd
          handleOutput <- hGetContents readEnd
          (output, handleOutput) `shouldBe` (cs "foo\n", "foo\n")
          writePythonScript "exe" "print('foo', file=sys.stderr); sys.exit(42)"
          try (wrapper $ cmd "./exe" :: IO ())
            `shouldReturn` Left (ErrorCall "stderr:\nfoo\n")

    it "allows capturing both stdout and stderr" $ do
      writePythonScript "exe" "print('out') ; print('err', file=sys.stderr)"
      (StdoutUntrimmed out, StderrRaw err) <- run $ cmd "./exe"
      (out, err) `shouldBe` (cs "out\n", cs "err\n")

    describe "exitcodes" $ do
      it "throws when the exitcode is not 0" $ do
        writePythonScript "exe" "sys.exit(42)"
        run_ (cmd "./exe")
          `shouldThrowShow` "command failed with exitcode 42: ./exe"

      it "doesn't throw when the exitcode is captured" $ do
        writePythonScript "exe" "sys.exit(42)"
        exitCode <- run $ cmd "./exe"
        exitCode `shouldBe` ExitFailure 42

      it "captures success exitcodes" $ do
        writePythonScript "exe" "pass"
        run (cmd "./exe") `shouldReturn` ExitSuccess

      it "allows capturing exitcodes and stdout" $ do
        writePythonScript "exe" "print('foo'); sys.exit(42)"
        (exitCode, StdoutTrimmed stdout) <- run $ cmd "./exe"
        stdout `shouldBe` cs "foo"
        exitCode `shouldBe` ExitFailure 42

    describe "working directory" $ do
      it "inherits the current working directory" $ do
        writePythonScript "exe" "print(os.getcwd())"
        StdoutTrimmed stdout <- run $ cmd "./exe"
        cwd <- getCurrentDirectory
        stdout `shouldBe` cs cwd

      it "allows setting the working directory" $ do
        writePythonScript "exe" "print(os.getcwd())"
        createDirectory "dir"
        cwd <- getCurrentDirectory
        StdoutTrimmed stdout <-
          run $
            cmd (cwd </> "./exe")
              & setWorkingDir "dir"
        stdout `shouldBe` cs (cwd </> "dir")

      it "does not modify the parent's working directory" $ do
        writePythonScript "exe" "print('foo')"
        createDirectory "dir"
        before <- getCurrentDirectory
        StdoutTrimmed output <-
          run $
            cmd (before </> "./exe")
              & setWorkingDir "dir"
        after <- getCurrentDirectory
        output `shouldBe` cs "foo"
        after `shouldBe` before

    describe "when receiving an async exception" $ do
      it "sends a signal to the child process" $ do
        writePythonScript "exe" $
          unindent
            [i|
              import signal
              import time
              import os

              def handler(signal, stack_frame):
                open('received-signal', 'a').write(f'received signal: {signal}')
                exit(0)

              signal.signal(signal.SIGTERM, handler)
              open('running', 'a').write('')
              time.sleep(3)
            |]
        thread <- forkIO $ run_ $ cmd "./exe"
        waitFor $ void $ readFile "running"
        killThread thread
        waitFor $ do
          readFile "received-signal" `shouldReturn` "received signal: 15"

    describe "environment" $ do
      it "inherits the environment (by default)" $ do
        withModifiedEnvironment [("TEST_ENV_VAR", "foo")] $ do
          writePythonScript "exe" "print(os.environ.get('TEST_ENV_VAR'))"
          StdoutTrimmed stdout <- run $ cmd "./exe"
          stdout `shouldBe` cs "foo"

      describe "modifyEnvVar" $ do
        it "allows setting environment variables" $ do
          writePythonScript "exe" "print(os.environ.get('TEST_ENV_VAR'))"
          StdoutTrimmed stdout <-
            run $
              cmd "./exe"
                & modifyEnvVar "TEST_ENV_VAR" (const $ Just "foo")
          stdout `shouldBe` cs "foo"

        it "doesn't overwrite existing environment variables" $ do
          withModifiedEnvironment [("TEST_ENV_VAR", "foo")] $ do
            writePythonScript "exe" "print(os.environ.get('TEST_ENV_VAR'))"
            StdoutTrimmed stdout <-
              run $
                cmd "./exe"
                  & modifyEnvVar "OTHER_ENV_VAR" (const $ Just "bar")
            stdout `shouldBe` cs "foo"

        it "allows modifying environment variables" $ do
          withModifiedEnvironment [("TEST_ENV_VAR", "foo"), ("OTHER_VAR", "bar")] $ do
            writePythonScript "exe" $
              unindent
                [i|
                  print(os.environ.get('TEST_ENV_VAR'))
                  print(os.environ.get('OTHER_VAR'))
                |]
            StdoutTrimmed stdout <-
              run $
                cmd "./exe"
                  & modifyEnvVar "TEST_ENV_VAR" (fmap reverse)
            stdout `shouldBe` cs "oof\nbar"

        it "allows removing environment variables" $ do
          withModifiedEnvironment [("TEST_ENV_VAR", "foo")] $ do
            writePythonScript "exe" "print(os.environ.get('TEST_ENV_VAR'))"
            StdoutTrimmed stdout <-
              run $
                cmd "./exe"
                  & modifyEnvVar "TEST_ENV_VAR" (const Nothing)
            stdout `shouldBe` cs "None"

        it "allows setting multiple environment variables" $ do
          writePythonScript "exe" $
            unindent
              [i|
                print(os.environ.get('TEST_ENV_VAR'))
                print(os.environ.get('OTHER_VAR'))
              |]
          StdoutTrimmed stdout <-
            run $
              cmd "./exe"
                & modifyEnvVar "TEST_ENV_VAR" (const $ Just "foo")
                & modifyEnvVar "OTHER_VAR" (const $ Just "bar")
          stdout `shouldBe` cs "foo\nbar"

        it "allows modifying environment variables multiple times" $ do
          writePythonScript "exe" "print(os.environ.get('TEST_ENV_VAR'))"
          StdoutTrimmed stdout <-
            run $
              cmd "./exe"
                & modifyEnvVar "TEST_ENV_VAR" (const $ Just "foo")
                & modifyEnvVar "TEST_ENV_VAR" (fmap reverse)
          stdout `shouldBe` cs "oof"

writePythonScript :: FilePath -> String -> IO ()
writePythonScript file code = do
  pythonPath <- getEnv "PYTHON_BIN_PATH"
  writeFile file ("#!" <> pythonPath <> "\nimport os\nimport sys\n" <> code)
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

waitFor :: IO () -> IO ()
waitFor action = go 10
  where
    go n = do
      mException :: Either SomeException () <- try action
      case mException of
        Right () -> return ()
        Left exception ->
          if n <= 0
            then throwIO $ ErrorCall $ "waitFor timed out: " <> show exception
            else do
              threadDelay 100000
              go (n - 1)
