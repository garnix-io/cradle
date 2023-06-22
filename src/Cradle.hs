{-# LANGUAGE FlexibleInstances #-}

module Cradle (Runnable (..)) where

import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit
import System.Process

class Runnable runnable where
  run :: (MonadIO m) => runnable -> m ()

instance Runnable String where
  run executable = run (executable, [] :: [String])

instance Runnable (String, String) where
  run (executable, arg) = run (executable, [arg])

instance Runnable (String, [String]) where
  run (executable, args) = liftIO $ do
    let cp =
          (proc executable args)
            { std_out = Inherit,
              std_err = Inherit
            }
    (_, _, _, handle) <- createProcess cp
    exitCode <- waitForProcess handle
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure exitCode ->
        throwIO $
          ErrorCall $
            "command failed with exitcode "
              <> show exitCode
              <> ": "
              <> executable
