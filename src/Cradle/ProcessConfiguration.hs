{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cradle.ProcessConfiguration
  ( ProcessConfiguration (..),
    StdinConfig (..),
    OutputStreamConfig (..),
    silenceDefault,
    addHandle,
    cmd,
    ProcessResult (..),
    runProcess,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Cradle.SafeUnix
import Data.ByteString (ByteString, hGetContents, hGetSome, hPut, null)
import System.Environment (getEnvironment)
import System.Exit
import System.IO (Handle, hClose)
import System.Posix.Internals (hostIsThreaded)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    cleanupProcess,
    createProcess_,
    proc,
    waitForProcess,
  )

data ProcessConfiguration = ProcessConfiguration
  { executable :: String,
    arguments :: [String],
    environmentModification :: Maybe ([(String, String)] -> [(String, String)]),
    workingDir :: Maybe FilePath,
    throwOnError :: Bool,
    stdinConfig :: StdinConfig,
    stdoutConfig :: OutputStreamConfig,
    stderrConfig :: OutputStreamConfig,
    delegateCtlc :: Bool
  }

data StdinConfig
  = InheritStdin
  | UseStdinHandle Handle
  | StdinString ByteString
  | NoStdinStream

data OutputStreamConfig = OutputStreamConfig
  { capture :: Bool,
    -- | Handles that the user set for the output stream.
    --
    -- @Nothing@ means use the default behavior (which depends on the @capture@
    -- field).
    setHandles :: Maybe [Handle]
  }
  deriving stock (Show)

defaultOutputStreamConfig :: OutputStreamConfig
defaultOutputStreamConfig = OutputStreamConfig False Nothing

silenceDefault :: OutputStreamConfig -> OutputStreamConfig
silenceDefault config =
  config
    { setHandles = case setHandles config of
        Nothing -> Just []
        Just handles -> Just handles
    }

addHandle :: Handle -> OutputStreamConfig -> OutputStreamConfig
addHandle handle config =
  config
    { setHandles = case setHandles config of
        Nothing -> Just [handle]
        Just hs -> Just $ handle : hs
    }

cmd :: String -> ProcessConfiguration
cmd executable =
  ProcessConfiguration
    { executable = executable,
      arguments = [],
      environmentModification = Nothing,
      workingDir = Nothing,
      throwOnError = True,
      stdinConfig = InheritStdin,
      stdoutConfig = defaultOutputStreamConfig,
      stderrConfig = defaultOutputStreamConfig,
      delegateCtlc = False
    }

data ProcessResult = ProcessResult
  { exitCode :: ExitCode,
    stdout :: Maybe ByteString,
    stderr :: Maybe ByteString,
    processConfiguration :: ProcessConfiguration
  }

runProcess :: ProcessConfiguration -> IO ProcessResult
runProcess config = do
  assertThreadedRuntime
  let stdoutHandler = outputStreamHandler $ stdoutConfig config
      stderrHandler = outputStreamHandler $ stderrConfig config
  environment <- forM (environmentModification config) $ \f -> do
    f <$> getEnvironment
  withStdinStdStream (stdinConfig config) $ \stdinStdStream -> do
    withCreateProcess
      "Cradle.run"
      ( (proc (executable config) (arguments config))
          { cwd = workingDir config,
            std_in = stdinStdStream,
            std_out = stdStream stdoutHandler,
            std_err = stdStream stderrHandler,
            delegate_ctlc = delegateCtlc config,
            env = environment
          }
      )
      $ \_ mStdout mStderr handle -> do
        waitForStdoutCapture <- startCapturing stdoutHandler mStdout
        waitForStderrCapture <- startCapturing stderrHandler mStderr
        exitCode <- waitForProcess handle
        throwWhenNonZero config exitCode
        stdout <- waitForStdoutCapture
        stderr <- waitForStderrCapture
        return $
          ProcessResult
            { stdout,
              stderr,
              exitCode,
              processConfiguration = config
            }

withCreateProcess :: String -> CreateProcess -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withCreateProcess message createProcess action =
  bracket
    (createProcess_ message createProcess)
    cleanupProcess
    (\(mStdin, mStdout, mStderr, processHandle) -> action mStdin mStdout mStderr processHandle)

withStdinStdStream :: StdinConfig -> (StdStream -> IO a) -> IO a
withStdinStdStream con action = do
  bracket start end $ \(stdStream, _) -> action stdStream
  where
    start :: IO (StdStream, Maybe (Handle, ThreadId))
    start = case con of
      InheritStdin -> return (Inherit, Nothing)
      UseStdinHandle handle -> return (UseHandle handle, Nothing)
      NoStdinStream -> return (NoStream, Nothing)
      StdinString stdin -> do
        (readEnd, writeEnd) <- safeCreatePipe
        thread <- forkIO $ do
          hPut writeEnd stdin
          hClose writeEnd
        return (UseHandle readEnd, Just (writeEnd, thread))
    end :: (StdStream, Maybe (Handle, ThreadId)) -> IO ()
    end (_, m) = forM_ m $ \(handle, thread) -> do
      hClose handle
      throwTo thread (ErrorCall "todo")

data OutputStreamHandler = OutputStreamHandler
  { stdStream :: StdStream,
    startCapturing :: Maybe Handle -> IO (IO (Maybe ByteString))
  }

maxBufferSize :: Int
maxBufferSize = 1024 * 1024

outputStreamHandler :: OutputStreamConfig -> OutputStreamHandler
outputStreamHandler config =
  OutputStreamHandler
    { stdStream = case config of
        OutputStreamConfig False Nothing -> Inherit
        OutputStreamConfig False (Just [sink]) -> UseHandle sink
        OutputStreamConfig _ _ -> CreatePipe,
      startCapturing = case config of
        OutputStreamConfig False Nothing -> expectNoHandle $ return $ return Nothing
        OutputStreamConfig True Nothing -> expectHandle $ \handle -> do
          mvar <- newEmptyMVar
          _ <- forkIO $ hGetContents handle >>= putMVar mvar
          return $ Just <$> readMVar mvar
        OutputStreamConfig False (Just []) -> expectHandle $ \_handle -> return $ return Nothing
        OutputStreamConfig False (Just [_sink]) -> expectNoHandle $ return $ return Nothing
        OutputStreamConfig False (Just sinks) -> expectHandle $ \handle -> do
          mvar <- newEmptyMVar
          _ <- forkIO $ do
            let loop = do
                  chunk <- hGetSome handle maxBufferSize
                  if Data.ByteString.null chunk
                    then return ()
                    else do
                      forM_ sinks $ \sink -> hPut sink chunk
                      loop
            loop
            putMVar mvar ()
          return $ do
            readMVar mvar
            return Nothing
        OutputStreamConfig True (Just sinks) -> expectHandle $ \handle -> do
          mvar <- newEmptyMVar
          _ <- forkIO $ do
            let loop acc = do
                  chunk <- hGetSome handle maxBufferSize
                  if Data.ByteString.null chunk
                    then return acc
                    else do
                      forM_ sinks $ \sink -> hPut sink chunk
                      loop $ acc <> chunk
            loop mempty >>= putMVar mvar
          return $ Just <$> readMVar mvar
    }
  where
    expectNoHandle :: IO a -> Maybe Handle -> IO a
    expectNoHandle action = \case
      Just _ -> throwIO $ ErrorCall "outputStreamHandler: pipe created unexpectedly"
      Nothing -> action

    expectHandle :: (Handle -> IO a) -> Maybe Handle -> IO a
    expectHandle action = \case
      Nothing -> throwIO $ ErrorCall "outputStreamHandler: pipe not created unexpectedly"
      Just handle -> action handle

assertThreadedRuntime :: IO ()
assertThreadedRuntime =
  when (not hostIsThreaded) $ do
    throwIO $ ErrorCall "Cradle needs the ghc's threaded runtime system to work correctly. Use the ghc option '-threaded'."

throwWhenNonZero :: ProcessConfiguration -> ExitCode -> IO ()
throwWhenNonZero config exitCode = do
  when (throwOnError config) $ do
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure exitCode -> do
        throwIO $
          ErrorCall $
            "command failed with exitcode "
              <> show exitCode
              <> ": "
              <> executable config
