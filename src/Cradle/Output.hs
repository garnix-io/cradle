{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cradle.Output
  ( runAndGetOutput,
    Output (..),
    StdoutUntrimmed (..),
    StdoutTrimmed (..),
    Stderr (..),
  )
where

import Control.Arrow ((>>>))
import Cradle.ProcessConfiguration
import Data.ByteString.Char8
import Data.Char
import Data.Proxy
import System.Exit
import Prelude hiding (dropWhile)

runAndGetOutput :: forall output. (Output output) => ProcessConfiguration -> IO output
runAndGetOutput config = extractOutput <$> runProcess (configure (Proxy :: Proxy output) config)

class Output output where
  configure :: Proxy output -> ProcessConfiguration -> ProcessConfiguration
  extractOutput :: ProcessResult -> output

instance Output () where
  configure Proxy = id
  extractOutput = const ()

instance (Output a, Output b) => Output (a, b) where
  configure Proxy =
    configure (Proxy :: Proxy a)
      >>> configure (Proxy :: Proxy b)
  extractOutput result = (extractOutput result, extractOutput result)

instance (Output a, Output b, Output c) => Output (a, b, c) where
  configure Proxy =
    configure (Proxy :: Proxy a)
      >>> configure (Proxy :: Proxy b)
      >>> configure (Proxy :: Proxy c)
  extractOutput result = (extractOutput result, extractOutput result, extractOutput result)

newtype StdoutUntrimmed = StdoutUntrimmed
  { fromStdoutUntrimmed :: ByteString
  }

instance Output StdoutUntrimmed where
  configure Proxy config = config {captureStdout = True}
  extractOutput result =
    case stdout result of
      Nothing -> error "impossible: stdout not captured"
      Just stdout -> StdoutUntrimmed stdout

newtype StdoutTrimmed = StdoutTrimmed
  { fromStdoutTrimmed :: ByteString
  }

instance Output StdoutTrimmed where
  configure Proxy config = config {captureStdout = True}
  extractOutput result =
    let StdoutUntrimmed output = extractOutput result
     in StdoutTrimmed $ trim $ output
    where
      trim = dropWhile isSpace . dropWhileEnd isSpace

newtype Stderr = Stderr
  { fromStderr :: ByteString
  }

instance Output Stderr where
  configure Proxy config = config {captureStderr = CaptureStream}
  extractOutput result =
    case stderr result of
      Nothing -> error "impossible: stderr not captured"
      Just stderr -> Stderr stderr

instance Output ExitCode where
  configure Proxy config = config {throwOnError = False}
  extractOutput = exitCode
