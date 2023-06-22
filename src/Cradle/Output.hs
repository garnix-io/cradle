{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Cradle.Output (Output (..), StdoutTrimmed (..)) where

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
    throwWhenNonZero config exitCode

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
    throwWhenNonZero config exitCode
    readMVar stdoutMVar

newtype StdoutTrimmed = StdoutTrimmed String

instance Output StdoutTrimmed where
  runAndGetOutput :: ProcessConfiguration -> IO StdoutTrimmed
  runAndGetOutput config = do
    StdoutTrimmed . trim <$> runAndGetOutput config
    where
      trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

instance Output ExitCode where
  runAndGetOutput :: ProcessConfiguration -> IO ExitCode
  runAndGetOutput config = do
    (_, _, _, handle) <- createProcess $ toCreateProcess config
    waitForProcess handle

throwWhenNonZero :: ProcessConfiguration -> ExitCode -> IO ()
throwWhenNonZero (ProcessConfiguration executable _) exitCode = do
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure exitCode -> do
      throwIO $
        ErrorCall $
          "command failed with exitcode "
            <> show exitCode
            <> ": "
            <> executable
