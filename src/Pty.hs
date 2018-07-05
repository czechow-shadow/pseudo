module Pty
  ( PtyData (..)
  , forkExecWithPty
  , close
  ) where

import           Protolude

import           System.Posix.IO          (closeFd)
import           System.Posix.Types       (CPid (..), Fd (..))
import           System.Process           (ProcessHandle,
                                           interruptProcessGroupOf,
                                           waitForProcess)
import           System.Process.Internals (mkProcessHandle)

import           Foreign
import           Foreign.C.Error          (throwErrnoIfMinus1Retry)
import           Foreign.C.Types          (CInt (..))


newtype PtyData = PtyData { unPtyData :: ByteString } deriving (Eq, Show)

type CIntPty = CInt
type CIntPid = CInt


foreign import ccall "fork_exec_with_pty.h fork_exec_with_pty"
     c_fork_exec_with_pty :: Ptr CIntPty -> IO CIntPid

forkExecWithPty :: IO (Fd, ProcessHandle)
forkExecWithPty = do
  alloca $ \ptyPtr -> do
    pid <- throwErrnoIfMinus1Retry "failed to fork or open pty" $
                                   c_fork_exec_with_pty ptyPtr
    pty <- peek ptyPtr
    ph <- mkProcessHandle (CPid $ fromIntegral pid) True
    pure (Fd pty, ph)

close :: (Fd, ProcessHandle) -> IO ()
close (fd, ph) = closeFd fd `finally` close'
  where
    close' = do interruptProcessGroupOf ph
                waitForProcess ph
