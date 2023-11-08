{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Cradle.Input
  ( Input (..),
    StdinHandle (..),
    NoStdin (..),
    StdoutHandle (..),
    StderrHandle (..),
  )
where

import Cradle.ProcessConfiguration
import Data.List
import Data.Text (Text, unpack)
import System.IO (Handle)

class Input input where
  configureProcess :: input -> ProcessConfiguration -> ProcessConfiguration

instance Input String where
  configureProcess s config = case executable config of
    Nothing -> config {executable = Just s}
    Just _ -> addArgument s config

instance Input Text where
  configureProcess s = configureProcess (unpack s)

instance Input () where
  configureProcess () = id

instance (Input a, Input b) => Input (a, b) where
  configureProcess (a, b) = configureProcess b . configureProcess a

instance {-# OVERLAPS #-} (Input input) => Input [input] where
  configureProcess list config =
    foldl' (flip configureProcess) config list

newtype StdinHandle = StdinHandle Handle

instance Input StdinHandle where
  configureProcess (StdinHandle handle) config =
    config {stdinConfig = UseStdinHandle handle}

data NoStdin = NoStdin

instance Input NoStdin where
  configureProcess NoStdin config =
    config {stdinConfig = NoStdinStream}

newtype StdoutHandle = StdoutHandle Handle

instance Input StdoutHandle where
  configureProcess (StdoutHandle handle) config =
    config {stdoutConfig = PipeStream handle}

newtype StderrHandle = StderrHandle Handle

instance Input StderrHandle where
  configureProcess (StderrHandle handle) config =
    config {stderrConfig = PipeStream handle}
