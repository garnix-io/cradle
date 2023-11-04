{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Cradle.Input (Input (..), StderrHandle (..)) where

import Cradle.ProcessConfiguration
import Data.List
import Data.Text (Text, unpack)
import System.IO (Handle)

class Input input where
  configureProcess :: input -> Maybe ProcessConfiguration -> Maybe ProcessConfiguration

instance Input String where
  configureProcess s = \case
    Nothing -> Just $ defaultProcessConfiguration s
    Just config -> Just $ addArgument s config

instance Input Text where
  configureProcess s = configureProcess (unpack s)

instance Input () where
  configureProcess () = id

instance (Input a, Input b) => Input (a, b) where
  configureProcess (a, b) = configureProcess b . configureProcess a

instance {-# OVERLAPS #-} (Input input) => Input [input] where
  configureProcess list config =
    foldl' (flip configureProcess) config list

newtype StderrHandle = StderrHandle Handle

instance Input StderrHandle where
  configureProcess (StderrHandle handle) = fmap $ \config ->
    config {captureStderr = PipeStream handle}
