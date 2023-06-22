{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Cradle.Input (Input (..)) where

import Cradle.ProcessConfiguration
import Data.List

class Input input where
  configureProcess :: input -> Maybe ProcessConfiguration -> Maybe ProcessConfiguration

instance Input String where
  configureProcess :: String -> Maybe ProcessConfiguration -> Maybe ProcessConfiguration
  configureProcess s = \case
    Nothing ->
      Just $ ProcessConfiguration s []
    Just (ProcessConfiguration exe args) ->
      Just $ ProcessConfiguration exe (args ++ [s])

instance Input () where
  configureProcess () = id

instance (Input a, Input b) => Input (a, b) where
  configureProcess (a, b) = configureProcess b . configureProcess a

instance {-# OVERLAPS #-} (Input input) => Input [input] where
  configureProcess list config =
    foldl' (flip configureProcess) config list
