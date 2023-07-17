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
  ( module Cradle,
    Output,
    Input,
    StdoutTrimmed (..),
  )
where

import Control.Exception (ErrorCall (..), throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Cradle.Input
import Cradle.Output
import Cradle.ProcessConfiguration
import Data.Kind
import Data.Proxy
import GHC.TypeError (ErrorMessage (..), TypeError)

run_ ::
  (Runnable runnable, ReturnsUnit runnable) =>
  runnable
run_ = run

class ReturnsUnit a

instance {-# OVERLAPPING #-} (ReturnsUnit b) => ReturnsUnit (a -> b)

instance {-# OVERLAPPABLE #-} (Monad m, a ~ ()) => ReturnsUnit (m a)

type Runnable runnable =
  (RunProcessConfig runnable, Check (DefinesExecutable runnable))

run :: (Runnable runnable) => runnable
run = runProcessConfig Nothing

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
