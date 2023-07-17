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
import GHC.TypeError (ErrorMessage (..), TypeError)
import System.Exit (ExitCode (..))

class ReturnsUnit a

instance {-# OVERLAPPING #-} (ReturnsUnit b) => ReturnsUnit (a -> b)

instance {-# OVERLAPPABLE #-} (Monad m, a ~ ()) => ReturnsUnit (m a)

type Runnable runnable =
  (RunProcessConfig runnable, Check (DefinesExecutable runnable))

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
-- >>> run_ "echo" (words "foo bar")
-- foo bar
run :: (Runnable runnable) => runnable
run = runProcessConfig Nothing

-- | Same as `run`, but always returns '()'.
--
-- >>> run_ "echo" "Hello, World!"
-- Hello, World!
run_ ::
  (Runnable runnable, ReturnsUnit runnable) =>
  runnable
run_ = run

class RunProcessConfig runnable where
  runProcessConfig :: Maybe ProcessConfiguration -> runnable

instance
  (Input input, RunProcessConfig runnable) =>
  RunProcessConfig (input -> runnable)
  where
  runProcessConfig :: Maybe ProcessConfiguration -> input -> runnable
  runProcessConfig createProcess input =
    runProcessConfig (configureProcess input createProcess)

instance {-# OVERLAPS #-} forall m a. (MonadIO m, Output a) => RunProcessConfig (m a) where
  runProcessConfig :: Maybe ProcessConfiguration -> m a
  runProcessConfig = \case
    Nothing -> liftIO $ throwIO $ ErrorCall "should be impossible, see DefinesExecutable"
    Just config -> liftIO $ runAndGetOutput config

type family Check (bool :: Bool) :: Constraint where
  Check True = ()
  Check False =
    TypeError
      ( Text
          "`run` has to be passed at least one string as the executable"
      )

type family DefinesExecutable (runnable :: Type) :: Bool where
  DefinesExecutable (input -> runnable) =
    ContainsExecutable input || DefinesExecutable runnable
  DefinesExecutable runnable = False

type family ContainsExecutable (input :: Type) :: Bool where
  ContainsExecutable String = True
  ContainsExecutable (a, b) =
    ContainsExecutable a || ContainsExecutable b
  ContainsExecutable input = False

type family (||) (a :: Bool) (b :: Bool) :: Bool where
  False || False = False
  a || b = True
