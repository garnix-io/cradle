{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module TestUtils
  ( shouldTerminate,
    safeCreatePipe,
  )
where

import Foreign (Ptr, Storable (..), allocaArray)
import Foreign.C (CInt (..), throwErrnoIfMinus1_)
import System.IO (Handle)
import System.Posix.IO (FdOption (..), fdToHandle, setFdOption)
import qualified System.Posix.IO
import System.Posix.Types (Fd (..))
import qualified System.Process
import System.Timeout (timeout)
import Test.Hspec
import Prelude hiding (getContents, length)

shouldTerminate :: IO () -> IO ()
shouldTerminate action = do
  result <- timeout 1000000 action
  case result of
    Just () -> pure ()
    Nothing -> expectationFailure "shouldTerminate: did not terminate"

-- | Creates a pipe, while preventing child processes from inheriting the file descriptors.
-- (At least trying to, see the darwin comment below.)
safeCreatePipe :: IO (Handle, Handle)

#ifdef linux_HOST_OS

safeCreatePipe = do
  (readfd, writefd) <- safeCreatePipeFd
  readh <- fdToHandle readfd
  writeh <- fdToHandle writefd
  return (readh, writeh)

-- O_CLOEXEC from fcntl.h. See `man 2 open` and `man 2 pipe`.
oCLOEXEC :: CInt
oCLOEXEC = 524288

safeCreatePipeFd :: IO (Fd, Fd)
safeCreatePipeFd =
  allocaArray 2 $ \p_fd -> do
    throwErrnoIfMinus1_ "safeCreatePipe" (c_pipe2 p_fd oCLOEXEC)
    rfd <- Fd <$> peekElemOff p_fd 0
    wfd <- Fd <$> peekElemOff p_fd 1
    return (rfd, wfd)

foreign import ccall unsafe "pipe2"
  c_pipe2 :: Ptr CInt -> CInt -> IO CInt

#else

-- On darwin there is no `pipe2`, so we use `fcntl` to set `CLOEXEC` after
-- creating the file descriptors. This is not thread-safe though, but it's the
-- best we can do AFAIK.

safeCreatePipe = do
  (readEnd, writeEnd) <- System.Posix.IO.createPipe
  setFdOption readEnd CloseOnExec True
  setFdOption writeEnd CloseOnExec True
  readEnd <- fdToHandle readEnd
  writeEnd <- fdToHandle writeEnd
  pure (readEnd, writeEnd)

#endif
