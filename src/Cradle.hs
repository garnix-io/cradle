module Cradle where

import System.Process

run :: String -> IO ()
run executable = do
  let cp =
        (proc executable [])
          { std_out = Inherit,
            std_err = Inherit
          }
  (_, _, _, handle) <- createProcess cp
  _ <- waitForProcess handle
  return ()
