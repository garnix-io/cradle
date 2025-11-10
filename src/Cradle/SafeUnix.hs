module Cradle.SafeUnix (safeCreatePipe) where

import Foreign
import Foreign.C
import System.IO (Handle)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd (..))

safeCreatePipe :: IO (Handle, Handle)
safeCreatePipe = do
  (readfd, writefd) <- safeCreatePipeFd
  readh <- fdToHandle readfd
  writeh <- fdToHandle writefd
  return (readh, writeh)

safeCreatePipeFd :: IO (Fd, Fd)
safeCreatePipeFd =
  allocaArray 2 $ \p_fd -> do
    throwErrnoIfMinus1_ "safeCreatePipe" (c_cloexec_pipe p_fd)
    rfd <- Fd <$> peekElemOff p_fd 0
    wfd <- Fd <$> peekElemOff p_fd 1
    return (rfd, wfd)

foreign import ccall unsafe "cloexec_pipe"
  c_cloexec_pipe :: Ptr CInt -> IO CInt
