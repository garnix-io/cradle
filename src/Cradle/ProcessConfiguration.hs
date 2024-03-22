{-# LANGUAGE NamedFieldPuns #-}

module Cradle.ProcessConfiguration
  ( ProcessConfiguration (..),
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
import System.Exit
import System.IO (Handle)
import System.Posix.Internals (hostIsThreaded)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess_,
    proc,
    waitForProcess,
  )

data ProcessConfiguration = ProcessConfiguration
  { executable :: Maybe String,
    arguments :: [String],
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
  | PipeStream Handle

cmd :: String -> ProcessConfiguration
cmd executable =
  ProcessConfiguration
    { executable = Just executable,
      arguments = [],
      workingDir = Nothing,
      throwOnError = True,
      stdinConfig = InheritStdin,
      stdoutConfig = InheritStream,
      stderrConfig = InheritStream,
      delegateCtlc = False
    }

data ProcessResult = ProcessResult
  { exitCode :: ExitCode,
    stdout :: Maybe ByteString,
    stderr :: Maybe ByteString
  }

runProcess :: ProcessConfiguration -> IO ProcessResult
runProcess config = do
  assertThreadedRuntime
  executable <- case executable config of
    Just executable -> return executable
    Nothing -> throwIO $ ErrorCall "Cradle: no executable given"
  (_, mStdout, mStderr, handle) <-
    createProcess_ "Cradle.run" $
      (proc executable (arguments config))
        { cwd = workingDir config,
          std_in = case stdinConfig config of
            InheritStdin -> Inherit
            UseStdinHandle handle -> UseHandle handle
            NoStdinStream -> NoStream,
          std_out = case stdoutConfig config of
            InheritStream -> Inherit
            CaptureStream -> CreatePipe
            PipeStream handle -> UseHandle handle,
          std_err = case stderrConfig config of
            InheritStream -> Inherit
            CaptureStream -> CreatePipe
            PipeStream handle -> UseHandle handle,
          delegate_ctlc = delegateCtlc config
        }
  mStdoutMVar <- forM mStdout $ \stdout -> do
    mvar <- newEmptyMVar
    _ <-
      forkIO $
        hGetContents stdout >>= putMVar mvar
    return mvar
  mStderrMVar <- forM mStderr $ \stderr -> do
    mvar <- newEmptyMVar
    _ <-
      forkIO $
        hGetContents stderr >>= putMVar mvar
    return mvar
  exitCode <- waitForProcess handle
  throwWhenNonZero executable config exitCode
  stdout <- forM mStdoutMVar readMVar
  stderr <- forM mStderrMVar readMVar
  return $
    ProcessResult
      { stdout,
        stderr,
        exitCode
      }

assertThreadedRuntime :: IO ()
assertThreadedRuntime =
  when (not hostIsThreaded) $ do
    throwIO $ ErrorCall "Cradle needs the ghc's threaded runtime system to work correctly. Use the ghc option '-threaded'."

throwWhenNonZero :: String -> ProcessConfiguration -> ExitCode -> IO ()
throwWhenNonZero executable config exitCode = do
  when (throwOnError config) $ do
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure exitCode -> do
        throwIO $
          ErrorCall $
            "command failed with exitcode "
              <> show exitCode
              <> ": "
              <> executable
