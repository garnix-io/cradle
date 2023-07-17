{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cradle
  ( run,
    run_,
    Runnable,

    -- * Possible Output
    Output,
    StdoutTrimmed (..),
    ExitCode (..),
  )
where

import Control.Exception (ErrorCall (..), throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Cradle.Input
import Cradle.Output
import Cradle.ProcessConfiguration
import Data.Kind
import System.Exit (ExitCode (..))

-- | Runs a child process and retrieves a result from it.
--
-- For example:
--
-- >>> stdout :: String <- run "echo" "Hello, World!"
-- >>> print stdout
-- "Hello, World!\n"
--
-- == Outputs
--
-- `run` is polymorphic in its output, the output type just has to implement 'Output'.
-- So for example you can get the exit code of a process like this:
--
-- >>> run "false" :: IO ExitCode
-- ExitFailure 1
--
-- If you don't want to retrieve any information from a child process, you can use `run_` (or make the output type '()').
--
-- For more information on available output types, see 'Output'.
--
-- == Inputs
--
-- 'run' and 'run_' are also variadic, i.e. it takes as many arguments as you want.
-- All arguments are polymorphic and have to implement 'Input'.
--
-- >>> run_ "echo" "foo" "bar"
-- foo bar
-- >>> run_ "echo"
-- <BLANKLINE>
--
-- == No Automatic Splitting of Strings, No Shells
--
-- `cradle` will not split any inputs by whitespace. So e.g. this doesn't work:
--
-- >>> run_ "echo foo"
-- *** Exception: echo foo: createProcess: posix_spawnp: does not exist (No such file or directory)
--
-- This is trying to run an executable with the file name @"echo foo"@, which doesn't exist.
-- You can split words in haskell though:
--
-- >>> run_ $ words "echo foo"
-- foo
run :: (Runnable runnable) => runnable
run = runProcessConfig Nothing

class Runnable runnable where
  runProcessConfig :: Maybe ProcessConfiguration -> runnable

instance
  (Input input, Runnable runnable) =>
  Runnable (input -> runnable)
  where
  runProcessConfig :: Maybe ProcessConfiguration -> input -> runnable
  runProcessConfig createProcess input =
    runProcessConfig (configureProcess input createProcess)

instance {-# OVERLAPS #-} forall m a. (MonadIO m, Output a) => Runnable (m a) where
  runProcessConfig :: Maybe ProcessConfiguration -> m a
  runProcessConfig = \case
    Nothing -> liftIO $ throwIO $ ErrorCall "should be impossible, see DefinesExecutable"
    Just config -> liftIO $ runAndGetOutput config

-- | Same as `run`, but always returns '()'.
--
-- >>> run_ "echo" "Hello, World!"
-- Hello, World!
run_ ::
  (Runnable runnable, ReturnsUnit runnable) =>
  runnable
run_ = run

class ReturnsUnit a

instance {-# OVERLAPPING #-} (ReturnsUnit b) => ReturnsUnit (a -> b)

instance {-# OVERLAPPABLE #-} (Monad m, a ~ ()) => ReturnsUnit (m a)
