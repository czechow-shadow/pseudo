module Pty
  ( PtyData (..)
  , forkExecWithPty
  , read
  , write
  , close
  ) where

import           Protolude

import           System.IO.Error          (ioeGetErrorType)
import           System.Posix.IO          (closeFd, fdReadBuf, fdWriteBuf)
import           System.Posix.Types       (CPid (..), Fd (..))
import           System.Process           (ProcessHandle, waitForProcess)
import           System.Process.Internals (mkProcessHandle)

import           Foreign                  hiding (void)
import           Foreign.C.Error          (throwErrnoIfMinus1Retry)
import           Foreign.C.Types
import           Foreign.Marshal.Array    (allocaArray)

import           GHC.IO.Exception         (IOErrorType (HardwareFault))

import qualified Data.ByteString          as BS


newtype PtyData = PtyData { unPtyData :: ByteString } deriving (Eq, Show)

type CIntPty = CInt
type CIntPid = CInt

-- FIXME: common?
bsize :: Int
bsize = 1024


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
close (fd, ph) = closeFd fd `finally` waitForProcess ph

read :: Fd -> IO (Maybe PtyData)
read fd =
  allocaArray bsize $ \buf -> readPty' buf
  where
    readPty' :: Ptr Word8 -> IO (Maybe PtyData)
    readPty' buf = do
      try (fdReadBuf fd buf $ fromIntegral bsize) >>= \case
        Right 0 -> pure Nothing
        Right c -> do a <- peekArray (fromIntegral c) buf
                      pure $ Just $ PtyData $ BS.pack a
        Left (e :: IOException) -> handleE e
        where
          handleE e -- handle pty slave termination gracefully
            | ioeGetErrorType e == HardwareFault = pure Nothing
            | otherwise                          = throwIO e


write :: PtyData -> Fd -> IO (Maybe ())
write (PtyData xs) fd | BS.length xs == 0 = pure $ Just ()
                      | otherwise = writePty' xs
  where
    writePty' :: ByteString -> IO (Maybe ())
    writePty' bs' = BS.useAsCStringLen bs' $ \(buf, len) -> do
      fdWriteBuf fd (castPtr buf) (fromIntegral len) >>= \case
        0 -> pure Nothing -- EOF
        c -> if  c == (fromIntegral len)
             then pure $ Just ()
             else writePty' $ BS.drop (fromIntegral c) bs'

