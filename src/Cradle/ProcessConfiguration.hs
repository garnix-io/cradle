{-# LANGUAGE NamedFieldPuns #-}

module Cradle.ProcessConfiguration where

import System.Process

data ProcessConfiguration = ProcessConfiguration
  { executable :: String,
    arguments :: [String]
  }

toCreateProcess :: ProcessConfiguration -> CreateProcess
toCreateProcess ProcessConfiguration {executable, arguments} =
  proc executable arguments
