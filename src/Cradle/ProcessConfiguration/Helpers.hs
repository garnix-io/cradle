{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

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

silenceStdout :: ProcessConfiguration -> ProcessConfiguration
silenceStdout config =
  config {stdoutConfig = IgnoreStream}

setStderrHandle :: Handle -> ProcessConfiguration -> ProcessConfiguration
setStderrHandle handle config =
  config {stderrConfig = PipeStream handle}

silenceStderr :: ProcessConfiguration -> ProcessConfiguration
silenceStderr config =
  config {stderrConfig = IgnoreStream}

setDelegateCtrlC :: ProcessConfiguration -> ProcessConfiguration
setDelegateCtrlC config =
  config {delegateCtlc = True}

modifyEnvVar ::
  String ->
  (Maybe String -> Maybe String) ->
  ProcessConfiguration ->
  ProcessConfiguration
modifyEnvVar name f config =
  config
    { environModification =
        Just $ maybe newModification (newModification .) $ environModification config
    }
  where
    newModification :: [(String, String)] -> [(String, String)]
    newModification env =
      maybe [] (pure . (name,)) (f $ lookup name env)
        ++ filter ((/= name) . fst) env

setWorkingDir :: FilePath -> ProcessConfiguration -> ProcessConfiguration
setWorkingDir dir config =
  config {workingDir = Just dir}
