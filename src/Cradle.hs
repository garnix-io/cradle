{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cradle where

import Control.Concurrent
import Control.Exception (ErrorCall (..), throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char
import Data.Kind
import Data.List (foldl')
import GHC.TypeError (ErrorMessage (..), TypeError)
import System.Exit
import System.IO
import System.Process

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
  (ProcessOption option, RunProcessConfig runnable) =>
  RunProcessConfig (option -> runnable)
  where
  runProcessConfig :: Maybe ProcessConfiguration -> option -> runnable
  runProcessConfig createProcess option =
    runProcessConfig (configureProcess option createProcess)

instance {-# OVERLAPS #-} (MonadIO m, Output a) => RunProcessConfig (m a) where
  runProcessConfig :: Maybe ProcessConfiguration -> m a
  runProcessConfig = \case
    Nothing -> liftIO $ throwIO $ ErrorCall "should be impossible, see DefinesExecutable"
    Just config -> liftIO $ runAndGetOutput config

class ProcessOption option where
  configureProcess :: option -> Maybe ProcessConfiguration -> Maybe ProcessConfiguration

instance ProcessOption String where
  configureProcess :: String -> Maybe ProcessConfiguration -> Maybe ProcessConfiguration
  configureProcess s = \case
    Nothing ->
      Just $ ProcessConfiguration s []
    Just (ProcessConfiguration exe args) ->
      Just $ ProcessConfiguration exe (args ++ [s])

instance ProcessOption () where
  configureProcess () = id

instance (ProcessOption a, ProcessOption b) => ProcessOption (a, b) where
  configureProcess (a, b) = configureProcess b . configureProcess a

instance {-# OVERLAPS #-} (ProcessOption option) => ProcessOption [option] where
  configureProcess list config =
    foldl' (flip configureProcess) config list

class Output output where
  runAndGetOutput :: ProcessConfiguration -> IO output

instance Output () where
  runAndGetOutput :: ProcessConfiguration -> IO ()
  runAndGetOutput config = do
    (_, _, _, handle) <- createProcess $ toCreateProcess config
    exitCode <- waitForProcess handle
    handleExitCode config exitCode

instance Output String where
  runAndGetOutput :: ProcessConfiguration -> IO String
  runAndGetOutput config = do
    (_, Just stdout, _, handle) <-
      createProcess
        (toCreateProcess config)
          { std_out = CreatePipe
          }
    stdoutMVar <- newEmptyMVar
    _ <-
      forkIO $
        hGetContents stdout >>= putMVar stdoutMVar
    exitCode <- waitForProcess handle
    handleExitCode config exitCode
    readMVar stdoutMVar

newtype StdoutTrimmed = StdoutTrimmed String

instance Output StdoutTrimmed where
  runAndGetOutput :: ProcessConfiguration -> IO StdoutTrimmed
  runAndGetOutput config = do
    StdoutTrimmed . trim <$> runAndGetOutput config
    where
      trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

handleExitCode :: ProcessConfiguration -> ExitCode -> IO ()
handleExitCode (ProcessConfiguration executable _) exitCode = do
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure exitCode -> do
      throwIO $
        ErrorCall $
          "command failed with exitcode "
            <> show exitCode
            <> ": "
            <> executable

data ProcessConfiguration = ProcessConfiguration
  { executable :: String,
    arguments :: [String]
  }

toCreateProcess :: ProcessConfiguration -> CreateProcess
toCreateProcess ProcessConfiguration {executable, arguments} =
  proc executable arguments

type family Check (bool :: Bool) :: Constraint where
  Check True = ()
  Check False =
    TypeError
      ( Text
          "`run` has to be passed at least one string as the executable"
      )

type family DefinesExecutable (runnable :: Type) :: Bool where
  DefinesExecutable (option -> runnable) =
    ContainsExecutable option || DefinesExecutable runnable
  DefinesExecutable runnable = False

type family ContainsExecutable (option :: Type) :: Bool where
  ContainsExecutable String = True
  ContainsExecutable (a, b) =
    ContainsExecutable a || ContainsExecutable b
  ContainsExecutable option = False

type family (||) (a :: Bool) (b :: Bool) :: Bool where
  False || False = False
  a || b = True
