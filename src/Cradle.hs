module Cradle where

import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit
import System.Process

run :: (MonadIO m) => String -> m ()
run executable = liftIO $ do
  let cp =
        (proc executable [])
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
