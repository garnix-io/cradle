{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Cradle.Input
  ( Input (..),
    StdinHandle (..),
    NoStdin (..),
    StdoutHandle (..),
    StderrHandle (..),
    SetEnv (..),
    AddToEnv (..),
    DelegateCtrlC (..),
    WorkingDir (..),
  )
where

import Control.Arrow ((>>>))
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

instance
  (Input a, Input b) =>
  Input (a, b)
  where
  configureProcess (a, b) =
    configureProcess a
      >>> configureProcess b

instance
  (Input a, Input b, Input c) =>
  Input (a, b, c)
  where
  configureProcess (a, b, c) =
    configureProcess a
      >>> configureProcess b
      >>> configureProcess c

instance
  (Input a, Input b, Input c, Input d) =>
  Input (a, b, c, d)
  where
  configureProcess (a, b, c, d) =
    configureProcess a
      >>> configureProcess b
      >>> configureProcess c
      >>> configureProcess d

instance
  (Input a, Input b, Input c, Input d, Input e) =>
  Input (a, b, c, d, e)
  where
  configureProcess (a, b, c, d, e) =
    configureProcess a
      >>> configureProcess b
      >>> configureProcess c
      >>> configureProcess d
      >>> configureProcess e

instance
  (Input a, Input b, Input c, Input d, Input e, Input f) =>
  Input (a, b, c, d, e, f)
  where
  configureProcess (a, b, c, d, e, f) =
    configureProcess a
      >>> configureProcess b
      >>> configureProcess c
      >>> configureProcess d
      >>> configureProcess e
      >>> configureProcess f

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

data DelegateCtrlC = DelegateCtrlC

instance Input DelegateCtrlC where
  configureProcess DelegateCtrlC config =
    config {delegateCtlc = True}

data WorkingDir = WorkingDir FilePath

instance Input WorkingDir where
  configureProcess (WorkingDir dir) config =
    config {workingDir = Just dir}

data SetEnv = SetEnv [(String, String)]

instance Input SetEnv where
  configureProcess (SetEnv env) config =
    config {environment = SetEnvironment env}

data AddToEnv = AddToEnv [(String, String)]

instance Input AddToEnv where
  configureProcess (AddToEnv env) config = config {environment = newEnv}
    where
      newEnv = case environment config of
        AddToInheritedEnvironment e -> AddToInheritedEnvironment (env <> e)
        SetEnvironment e -> SetEnvironment (env <> e)
