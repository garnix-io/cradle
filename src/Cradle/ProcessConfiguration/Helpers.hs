{-# LANGUAGE FlexibleContexts #-}

module Cradle.ProcessConfiguration.Helpers where

import Cradle.ProcessConfiguration
import Data.String.Conversions (ConvertibleStrings, cs)
import System.IO (Handle)

addArgs :: ConvertibleStrings s String => [s] -> ProcessConfiguration -> ProcessConfiguration
addArgs args config =
  config {arguments = arguments config <> map cs args}

setStdinHandle :: Handle -> ProcessConfiguration -> ProcessConfiguration
setStdinHandle handle config =
  config {stdinConfig = UseStdinHandle handle}

setNoStdin :: ProcessConfiguration -> ProcessConfiguration
setNoStdin config =
  config {stdinConfig = NoStdinStream}

setStdoutHandle :: Handle -> ProcessConfiguration -> ProcessConfiguration
setStdoutHandle handle config =
  config {stdoutConfig = PipeStream handle}

setStderrHandle :: Handle -> ProcessConfiguration -> ProcessConfiguration
setStderrHandle handle config =
  config {stderrConfig = PipeStream handle}

setDelegateCtrlC :: ProcessConfiguration -> ProcessConfiguration
setDelegateCtrlC config =
  config {delegateCtlc = True}

setWorkingDir :: FilePath -> ProcessConfiguration -> ProcessConfiguration
setWorkingDir dir config =
  config {workingDir = Just dir}
