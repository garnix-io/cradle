{-# LANGUAGE NamedFieldPuns #-}

module Cradle.ProcessConfiguration
  ( ProcessConfiguration (..),
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
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

data ProcessConfiguration = ProcessConfiguration
  { executable :: String,
    arguments :: [String],
    throwOnError :: Bool,
    captureStdout :: Bool,
    captureStderr :: Bool
  }

defaultProcessConfiguration :: String -> ProcessConfiguration
defaultProcessConfiguration s =
  ProcessConfiguration
    { executable = s,
      arguments = [],
      throwOnError = True,
      captureStdout = False,
      captureStderr = False
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
  (_, mStdout, mStderr, handle) <-
    createProcess $
      (proc (executable config) (arguments config))
        { std_out = if captureStdout config then CreatePipe else Inherit,
          std_err = if captureStderr config then CreatePipe else Inherit
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
  throwWhenNonZero config exitCode
  stdout <- forM mStdoutMVar readMVar
  stderr <- forM mStderrMVar readMVar
  return $
    ProcessResult
      { stdout,
        stderr,
        exitCode
      }

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
