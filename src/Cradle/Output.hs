{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Cradle.Output where

import Control.Concurrent
import Control.Exception
import Cradle.ProcessConfiguration
import Data.Char
import System.Exit
import System.IO
import System.Process

class Output output where
  runAndGetOutput :: ProcessConfiguration -> IO output

instance Output () where
  runAndGetOutput :: ProcessConfiguration -> IO ()
  runAndGetOutput config = do
    (_, _, _, handle) <- createProcess $ toCreateProcess config
    exitCode <- waitForProcess handle
    handleExitCode config exitCode

instance Output String where
  runAndGetOutput :: ProcessConfiguration -> IO String
  runAndGetOutput config = do
    (_, Just stdout, _, handle) <-
      createProcess
        (toCreateProcess config)
          { std_out = CreatePipe
          }
    stdoutMVar <- newEmptyMVar
    _ <-
      forkIO $
        hGetContents stdout >>= putMVar stdoutMVar
    exitCode <- waitForProcess handle
    handleExitCode config exitCode
    readMVar stdoutMVar

newtype StdoutTrimmed = StdoutTrimmed String

instance Output StdoutTrimmed where
  runAndGetOutput :: ProcessConfiguration -> IO StdoutTrimmed
  runAndGetOutput config = do
    StdoutTrimmed . trim <$> runAndGetOutput config
    where
      trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

handleExitCode :: ProcessConfiguration -> ExitCode -> IO ()
handleExitCode (ProcessConfiguration executable _) exitCode = do
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure exitCode -> do
      throwIO $
        ErrorCall $
          "command failed with exitcode "
            <> show exitCode
            <> ": "
            <> executable
