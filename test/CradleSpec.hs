{-# LANGUAGE ScopedTypeVariables #-}

module CradleSpec where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad.Trans.Identity
import Cradle
import Data.ByteString (length)
import Data.String.Conversions
import Data.Text (Text)
import GHC.IO.Exception
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hClose, hGetContents, hIsClosed, hPutStrLn, stderr)
import System.IO.Silently
import System.Process (createPipe)
import Test.Hspec
import Test.Mockery.Directory
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
                            ( ioe_filename e == Just "./exe"
                                && ioe_type e == PermissionDenied
                            )
                        )

      it "throws when the hashbang interpreter cannot be found" $ do
        writeFile "exe" "#!/does/not/exist\nfoo"
        makeExecutable "exe"
        run_ (cmd "./exe")
          `shouldThrow` ( \(e :: IOException) ->
                            ioe_filename e == Just "./exe"
                              && ioe_type e == NoSuchThing
                        )

    it "allows to be run in MonadIO contexts" $ do
      writePythonScript "exe" "pass"
      runIdentityT $ do
        _ :: StdoutUntrimmed <- run $ cmd "./exe"
        run_ $ cmd "./exe"

    describe "arguments" $ do
      let writeArgumentWriter = do
            writePythonScript "exe" $
              "open('file', 'w').write(' '.join(sys.argv[1:]))"

      it "allows to pass in an argument" $ do
        writeArgumentWriter
        run_ $
          cmd "./exe"
            & addArgs ["foo"]
        readFile "file" `shouldReturn` "foo"

      it "allows to pass in multiple arguments" $ do
        writeArgumentWriter
        run_ $
          cmd "./exe"
            & addArgs ["foo", "bar"]
        readFile "file" `shouldReturn` "foo bar"

      it "allows to split strings in haskell" $ do
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
        it "allows to pass in handle for stdin" $ do
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

      it "allows to disable inheriting stdin" $ do
        writePythonScript "exe" "print(sys.stdin)"
        StdoutTrimmed output <- run $ cmd "./exe" & setNoStdin
        output `shouldBe` cs "None"

    describe "capturing stdout" $ do
      it "allows to capture stdout" $ do
        writePythonScript "exe" "print('output')"
        StdoutUntrimmed stdout <- run $ cmd "./exe"
        stdout `shouldBe` cs "output\n"

      it "allows to capture stdout *and* pass in arguments" $ do
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

      it "does not relay stdout when it's captured (by default)" $ do
        writePythonScript "exe" "print('foo')"
        output <- capture_ $ do
          StdoutUntrimmed _ <- run $ cmd "./exe"
          return ()
        output `shouldBe` ""

      describe "StdoutTrimmed" $ do
        it "allows to capture the stripped stdout" $ do
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
        length output `shouldBe` 2 ^ (22 :: Int)

      describe "using handles" $ do
        it "allows to send stdout to a handle" $ do
          writePythonScript "exe" "print('foo')"
          (readEnd, writeEnd) <- createPipe
          run_ $ cmd "./exe" & setStdoutHandle writeEnd
          hClose writeEnd
          hGetContents readEnd `shouldReturn` cs "foo\n"

        it "does not relay stdout when it's captured (by default)" $ do
          writePythonScript "exe" "print('foo')"
          (_readEnd, writeEnd) <- createPipe
          stdout <- capture_ $ run_ $ cmd "./exe" & setStdoutHandle writeEnd
          hClose writeEnd
          stdout `shouldBe` ""

        it "doesn't close the handle after running the process" $ do
          writePythonScript "exe" "print(sys.argv[1])"
          (readEnd, writeEnd) <- createPipe
          run_ $
            cmd "./exe"
              & addArgs ["foo"]
              & setStdoutHandle writeEnd
          hIsClosed writeEnd `shouldReturn` False
          hIsClosed readEnd `shouldReturn` False
          run_ $
            cmd "./exe"
              & addArgs ["bar"]
              & setStdoutHandle writeEnd
          hClose writeEnd
          hGetContents readEnd `shouldReturn` cs "foo\nbar\n"

    describe "capture stderr" $ do
      it "allows to capture stderr" $ do
        writePythonScript "exe" "print('output', file=sys.stderr)"
        Stderr stderr <- run $ cmd "./exe"
        stderr `shouldBe` cs "output\n"

      it "relays stderr when it's not captured (by default)" $ do
        writePythonScript "exe" "print('foo', file=sys.stderr)"
        output <- hCapture_ [stderr] $ run_ $ cmd "./exe"
        output `shouldBe` "foo\n"

      it "does not relay stderr when it's captured (by default)" $ do
        writePythonScript "exe" "print('foo', file=sys.stderr)"
        output <- hCapture_ [stderr] $ do
          Stderr _ <- run $ cmd "./exe"
          return ()
        output `shouldBe` ""

      describe "using handles" $ do
        it "allows to send stderr to a handle" $ do
          writePythonScript "exe" "print('foo', file=sys.stderr)"
          (readEnd, writeEnd) <- createPipe
          run_ $
            cmd "./exe"
              & setStderrHandle writeEnd
          hClose writeEnd
          hGetContents readEnd `shouldReturn` cs "foo\n"

        it "does not relay stderr when it's captured (by default)" $ do
          writePythonScript "exe" "print('foo', file=sys.stderr)"
          (_readEnd, writeEnd) <- createPipe
          stderr <- hCapture_ [stderr] $ run_ $ cmd "./exe" & setStderrHandle writeEnd
          hClose writeEnd
          stderr `shouldBe` ""

        it "doesn't close the handle after running the process" $ do
          writePythonScript "exe" "print(sys.argv[1], file=sys.stderr)"
          (readEnd, writeEnd) <- createPipe
          run_ $
            cmd "./exe"
              & setStderrHandle writeEnd
              & addArgs ["foo"]
          hIsClosed writeEnd `shouldReturn` False
          hIsClosed readEnd `shouldReturn` False
          run_ $
            cmd "./exe"
              & setStderrHandle writeEnd
              & addArgs ["bar"]
          hClose writeEnd
          hGetContents readEnd `shouldReturn` cs "foo\nbar\n"

    it "allows to capture both stdout and stderr" $ do
      writePythonScript "exe" "print('out') ; print('err', file=sys.stderr)"
      (StdoutUntrimmed out, Stderr err) <- run $ cmd "./exe"
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

      it "allows to capture exitcodes and stdout" $ do
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

      it "allows to set the working directory" $ do
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
