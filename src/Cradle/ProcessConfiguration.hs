{-# LANGUAGE NamedFieldPuns #-}

module Cradle.ProcessConfiguration
  ( ProcessConfiguration (..),
    StdinConfig (..),
    OutputStreamConfig (..),
    defaultProcessConfiguration,
    addArgument,
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
import System.Process (CreateProcess (..), StdStream (..), createProcess_, proc, waitForProcess)

data ProcessConfiguration = ProcessConfiguration
  { executable :: Maybe String,
    arguments :: [String],
    throwOnError :: Bool,
    stdinConfig :: StdinConfig,
    stdoutConfig :: OutputStreamConfig,
    stderrConfig :: OutputStreamConfig
  }

data StdinConfig
  = InheritStdin
  | UseStdinHandle Handle

data OutputStreamConfig
  = CaptureStream
  | InheritStream
  | PipeStream Handle

defaultProcessConfiguration :: ProcessConfiguration
defaultProcessConfiguration =
  ProcessConfiguration
    { executable = Nothing,
      arguments = [],
      throwOnError = True,
      stdinConfig = InheritStdin,
      stdoutConfig = InheritStream,
      stderrConfig = InheritStream
    }

addArgument :: String -> ProcessConfiguration -> ProcessConfiguration
addArgument arg config =
  config
    { arguments = arguments config ++ [arg]
    }

data ProcessResult = ProcessResult
  { exitCode :: ExitCode,
    stdout :: Maybe ByteString,
    stderr :: Maybe ByteString
  }

runProcess :: ProcessConfiguration -> IO ProcessResult
runProcess config = do
  executable <- case executable config of
    Just executable -> return executable
    Nothing -> throwIO $ ErrorCall "Cradle: no executable given"
  (_, mStdout, mStderr, handle) <-
    createProcess_ "Cradle.run" $
      (proc executable (arguments config))
        { std_in = case stdinConfig config of
            InheritStdin -> Inherit
            UseStdinHandle handle -> UseHandle handle,
          std_out = case stdoutConfig config of
            InheritStream -> Inherit
            CaptureStream -> CreatePipe
            PipeStream handle -> UseHandle handle,
          std_err = case stderrConfig config of
            InheritStream -> Inherit
            CaptureStream -> CreatePipe
            PipeStream handle -> UseHandle handle
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
