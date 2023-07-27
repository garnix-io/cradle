{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cradle.Output
  ( runAndGetOutput,
    Output (..),
    StdoutUntrimmed (..),
    StdoutTrimmed (..),
  )
where

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
  configure Proxy =
    configure (Proxy :: Proxy a)
      >>> configure (Proxy :: Proxy b)
  extractOutput result = (extractOutput result, extractOutput result)

instance Output () where
  configure Proxy = id
  extractOutput = const ()

newtype StdoutUntrimmed = StdoutUntrimmed
  { fromStdoutUntrimmed :: String
  }

instance Output StdoutUntrimmed where
  configure Proxy config = config {captureStdout = True}
  extractOutput result =
    case stdout result of
      Nothing -> error "impossible: stdout not captured"
      Just stdout -> StdoutUntrimmed stdout

newtype StdoutTrimmed = StdoutTrimmed
  { fromStdoutTrimmed :: String
  }

instance Output StdoutTrimmed where
  configure Proxy config = config {captureStdout = True}
  extractOutput result =
    let StdoutUntrimmed output = extractOutput result
     in StdoutTrimmed $ trim $ output
    where
      trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

instance Output ExitCode where
  configure Proxy config = config {throwOnError = False}
  extractOutput = exitCode
