module Cradle.SafeUnix (safeCreatePipe) where

import Foreign
import Foreign.C
import System.IO (Handle)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd (..))

-- O_CLOEXEC from fcntl.h. See `man 2 open` and `man 2 pipe`.
oCLOEXEC :: CInt
oCLOEXEC = 524288

safeCreatePipe :: IO (Handle, Handle)
safeCreatePipe = do
  (readfd, writefd) <- safeCreatePipeFd
  readh <- fdToHandle readfd
  writeh <- fdToHandle writefd
  return (readh, writeh)

safeCreatePipeFd :: IO (Fd, Fd)
safeCreatePipeFd =
  allocaArray 2 $ \p_fd -> do
    throwErrnoIfMinus1_ "safeCreatePipe" (c_pipe2 p_fd oCLOEXEC)
    rfd <- Fd <$> peekElemOff p_fd 0
    wfd <- Fd <$> peekElemOff p_fd 1
    return (rfd, wfd)

foreign import ccall unsafe "pipe2"
  c_pipe2 :: Ptr CInt -> CInt -> IO CInt
