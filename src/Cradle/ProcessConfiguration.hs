{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cradle.ProcessConfiguration
  ( ProcessConfiguration (..),
    showCommand,
    StdinConfig (..),
    OutputStreamConfig (..),
    cmd,
    ProcessResult (..),
    runProcess,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString, hGetContents)
import System.Environment (getEnvironment)
import System.Exit
import System.IO (Handle)
import System.Posix.Internals (hostIsThreaded)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    cleanupProcess,
    createProcess_,
    proc,
    waitForProcess,
  )

data ProcessConfiguration = ProcessConfiguration
  { executable :: String,
    arguments :: [String],
    environmentModification :: Maybe ([(String, String)] -> [(String, String)]),
    workingDir :: Maybe FilePath,
    throwOnError :: Bool,
    stdinConfig :: StdinConfig,
    stdoutConfig :: OutputStreamConfig,
    stderrConfig :: OutputStreamConfig,
    delegateCtlc :: Bool
  }

data StdinConfig
  = InheritStdin
  | UseStdinHandle Handle
  | NoStdinStream

data OutputStreamConfig
  = CaptureStream
  | InheritStream
  | IgnoreStream
  | PipeStream Handle
  deriving stock (Show)

cmd :: String -> ProcessConfiguration
cmd executable =
  ProcessConfiguration
    { executable = executable,
      arguments = [],
      environmentModification = Nothing,
      workingDir = Nothing,
      throwOnError = True,
      stdinConfig = InheritStdin,
      stdoutConfig = InheritStream,
      stderrConfig = InheritStream,
      delegateCtlc = False
    }

showCommand :: ProcessConfiguration -> String
showCommand config = unwords (executable config : arguments config)

data ProcessResult = ProcessResult
  { exitCode :: ExitCode,
    stdout :: Maybe ByteString,
    stderr :: Maybe ByteString
  }

runProcess :: ProcessConfiguration -> IO ProcessResult
runProcess config = do
  assertThreadedRuntime
  let stdoutHandler = outputStreamHandler $ stdoutConfig config
      stderrHandler = outputStreamHandler $ stderrConfig config
  environment <- forM (environmentModification config) $ \f -> do
    f <$> getEnvironment
  withCreateProcess
    "Cradle.run"
    ( (proc (executable config) (arguments config))
        { cwd = workingDir config,
          std_in = case stdinConfig config of
            InheritStdin -> Inherit
            UseStdinHandle handle -> UseHandle handle
            NoStdinStream -> NoStream,
          std_out = stdStream stdoutHandler,
          std_err = stdStream stderrHandler,
          delegate_ctlc = delegateCtlc config,
          env = environment
        }
    )
    $ \_ mStdout mStderr handle -> do
      waitForStdoutCapture <- startCapturing stdoutHandler mStdout
      waitForStderrCapture <- startCapturing stderrHandler mStderr
      exitCode <- waitForProcess handle
      throwWhenNonZero config exitCode
      stdout <- waitForStdoutCapture
      stderr <- waitForStderrCapture
      return $
        ProcessResult
          { stdout,
            stderr,
            exitCode
          }

withCreateProcess :: String -> CreateProcess -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withCreateProcess message createProcess action =
  bracket
    (createProcess_ message createProcess)
    cleanupProcess
    (\(mStdin, mStdout, mStderr, processHandle) -> action mStdin mStdout mStderr processHandle)

data OutputStreamHandler = OutputStreamHandler
  { stdStream :: StdStream,
    startCapturing :: Maybe Handle -> IO (IO (Maybe ByteString))
  }

outputStreamHandler :: OutputStreamConfig -> OutputStreamHandler
outputStreamHandler config =
  OutputStreamHandler
    { stdStream = case config of
        InheritStream -> Inherit
        CaptureStream -> CreatePipe
        IgnoreStream -> CreatePipe
        PipeStream handle -> UseHandle handle,
      startCapturing = \mHandle -> case (config, mHandle) of
        (InheritStream, Nothing) -> return $ return Nothing
        (CaptureStream, Just handle) -> do
          mvar <- newEmptyMVar
          _ <- forkIO $ hGetContents handle >>= putMVar mvar . Just
          return $ readMVar mvar
        (IgnoreStream, Just _handle) -> return $ return Nothing
        (PipeStream _handle, Nothing) -> return $ return Nothing
        (_, Just _) -> throwIO $ ErrorCall "outputStreamHandler: pipe created unexpectedly"
        (_, Nothing) -> throwIO $ ErrorCall "outputStreamHandler: pipe not created unexpectedly"
    }

assertThreadedRuntime :: IO ()
assertThreadedRuntime =
  when (not hostIsThreaded) $ do
    throwIO $ ErrorCall "Cradle needs the ghc's threaded runtime system to work correctly. Use the ghc option '-threaded'."

throwWhenNonZero :: ProcessConfiguration -> ExitCode -> IO ()
throwWhenNonZero config exitCode = do
  when (throwOnError config) $ do
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure exitCode -> do
        throwIO $
          ErrorCall $
            "command failed with exitcode "
              <> show exitCode
              <> ": "
              <> executable config
