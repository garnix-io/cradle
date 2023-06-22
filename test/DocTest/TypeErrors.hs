{-# OPTIONS_GHC -Wno-unused-imports #-}

module DocTest.TypeErrors where

import Cradle

-- |
-- >>> run :: IO ()
-- ...
--     • `run` has to be passed at least one string as the executable
--     • In the expression: run :: IO ()
-- ...
--
-- >>> run_ :: IO ()
-- ...
--     • `run` has to be passed at least one string as the executable
--     • In the expression: run_ :: IO ()
-- ...
--
-- >>> run () :: IO ()
-- ...
--     • `run` has to be passed at least one string as the executable
--     • In the expression: run () :: IO ()
-- ...
--
-- >>> run ((), ()) :: IO ()
-- ...
--     • `run` has to be passed at least one string as the executable
--     • In the expression: run ((), ()) :: IO ()
-- ...
--
-- >>> run ("true", ()) :: IO ()
-- >>> run ((), "true") :: IO ()
-- >>> run () "true" :: IO ()
typeErrorDocTests :: ()
typeErrorDocTests = ()
