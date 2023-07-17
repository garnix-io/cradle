{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cradle.Output (runAndGetOutput, Output (..), StdoutTrimmed (..)) where

import Control.Arrow ((>>>))
import Cradle.ProcessConfiguration
import Data.Char
import Data.Proxy
import System.Exit

runAndGetOutput :: forall output. (Output output) => ProcessConfiguration -> IO output
runAndGetOutput config = extractOutput <$> runProcess (configure (Proxy :: Proxy output) config)

class Output output where
  configure :: Proxy output -> ProcessConfiguration -> ProcessConfiguration
  extractOutput :: ProcessResult -> output

instance (Output a, Output b) => Output (a, b) where
  configure :: Proxy (a, b) -> ProcessConfiguration -> ProcessConfiguration
  configure Proxy =
    configure (Proxy :: Proxy a)
      >>> configure (Proxy :: Proxy b)
  extractOutput result = (extractOutput result, extractOutput result)

instance Output () where
  configure :: Proxy () -> ProcessConfiguration -> ProcessConfiguration
  configure Proxy = id
  extractOutput = const ()

instance Output String where
  configure :: Proxy String -> ProcessConfiguration -> ProcessConfiguration
  configure Proxy config = config {captureStdout = True}
  extractOutput result =
    case stdout result of
      Nothing -> error "impossible: stdout not captured"
      Just stdout -> stdout

newtype StdoutTrimmed = StdoutTrimmed String

instance Output StdoutTrimmed where
  configure :: Proxy StdoutTrimmed -> ProcessConfiguration -> ProcessConfiguration
  configure Proxy config = config {captureStdout = True}
  extractOutput result =
    StdoutTrimmed $ trim $ extractOutput result
    where
      trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

instance Output ExitCode where
  configure :: Proxy ExitCode -> ProcessConfiguration -> ProcessConfiguration
  configure Proxy config = config {throwOnError = False}
  extractOutput = exitCode
