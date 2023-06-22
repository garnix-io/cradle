module Cradle where

import Control.Exception
import System.Exit
import System.Process

run :: String -> IO ()
run executable = do
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
