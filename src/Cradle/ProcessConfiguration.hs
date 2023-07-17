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
import System.Exit
import System.IO (hGetContents)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

data ProcessConfiguration = ProcessConfiguration
  { executable :: String,
    arguments :: [String],
    throwOnError :: Bool,
    captureStdout :: Bool
  }

defaultProcessConfiguration :: String -> ProcessConfiguration
defaultProcessConfiguration s = ProcessConfiguration s [] True False

addArgument :: String -> ProcessConfiguration -> ProcessConfiguration
addArgument arg config =
  config
    { arguments = arguments config ++ [arg]
    }

data ProcessResult = ProcessResult
  { exitCode :: ExitCode,
    stdout :: Maybe String
  }

runProcess :: ProcessConfiguration -> IO ProcessResult
runProcess config = do
  (_, mStdout, _, handle) <-
    createProcess $
      (proc (executable config) (arguments config))
        { std_out = if captureStdout config then CreatePipe else Inherit
        }
  mStdoutMVar <- forM mStdout $ \stdout -> do
    mvar <- newEmptyMVar
    _ <-
      forkIO $
        hGetContents stdout >>= putMVar mvar
    return mvar
  exitCode <- waitForProcess handle
  throwWhenNonZero config exitCode
  stdout <- forM mStdoutMVar readMVar
  return $
    ProcessResult
      { stdout,
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
