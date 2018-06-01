
module Lib where

import           Protolude

import           System.IO.Error          (ioeGetErrorType)
import           System.Posix.IO          (closeFd, fdReadBuf, fdWriteBuf)
import           System.Posix.Types       (CPid (..), Fd (..))
import           System.Process           (ProcessHandle, waitForProcess)
import           System.Process.Internals (mkProcessHandle)
import           System.Timeout           (timeout)

import           Foreign                  hiding (void)
import           Foreign.C.Error          (throwErrnoIfMinus1Retry)
import           Foreign.C.Types

-- import Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Marshal.Array    (allocaArray)
--import Data.Typeable

import           GHC.IO.Exception         (IOErrorType (HardwareFault))

-- import qualified Data.Text as T
-- import qualified Data.ByteString as BS

import           Network.Socket


type CIntPty = CInt
type CIntPid = CInt


bs :: Int
bs = 1024

foreign import ccall "fork_exec_with_pty.h fork_exec_with_pty"
     c_fork_exec_with_pty :: Ptr CIntPty -> IO CIntPid

fork_exec_with_pty :: IO (Fd, ProcessHandle)
fork_exec_with_pty = do
  alloca $ \ptyPtr -> do
    pid <- throwErrnoIfMinus1Retry "failed to fork or open pty" $
          c_fork_exec_with_pty ptyPtr
    pty <- peek ptyPtr
    ph <- mkProcessHandle (CPid $ fromIntegral pid) True
    pure (Fd pty, ph)


go :: IO ()
go = do
  putText "Listening"
  void $ timeout (1000 * 1000 * 5) $ bracket openSocket closeSocket acceptLoop
  putText "Sleeping"
  threadDelay $ 1000 * 1000 * 5
  go


openSocket :: IO Socket
openSocket = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  setSocketOption sock ReusePort 1
  bind sock $ SockAddrInet 3000 (tupleToHostAddress (0,0,0,0))
  listen sock 5
  pure sock

closeSocket :: Socket -> IO ()
closeSocket sock = close sock

acceptLoop :: Socket -> IO ()
acceptLoop sock = forever $ do
  (sock', peer) <- accept sock
  putText $ "Connection from " <> show peer
  async $ talk sock' `finally` close sock'

talk :: Socket -> IO ()
talk sock = do
  putText $ "Spawning bash and talking to it"
  bracket allocPty closePty $ \(fd, ph) ->
    withAsync (sockToPty sock fd) $ \as1 ->
    withAsync (ptyToSock sock fd) $ \as2 -> do
    void $ waitAny [as1, as2]
    void $ waitForProcess ph
  putText $ "Talk finished"
  where
    allocPty = fork_exec_with_pty
    closePty = closeFd . fst

sockToPty :: Socket -> Fd -> IO ()
sockToPty sock fd =
  allocaArray bs $ \buf -> sockToPty' buf
  where
    sockToPty' buf = do
      c <- recvBuf sock buf (fromIntegral bs)
      case c of
        0 -> do
          putText "Peer closed connection" -- FIXME: Use channels
          pure ()
        _ -> do _ <- fdWriteBuf fd buf (fromIntegral c) -- FIXME: check write cnt
                sockToPty' buf

ptyToSock :: Socket -> Fd -> IO ()
ptyToSock sock fd =
  allocaArray bs $ \buf -> ptyToSock' buf
  where
    ptyToSock' buf = do
      try (fdReadBuf fd buf $ fromIntegral bs) >>= \case
        Right r -> do
          -- putText $ "<=== Read " <> show r
  --        a <- peekArray (fromIntegral r) buf
          case r of
            0 -> putText "*** EOF ***"
            _ -> do sendAllBuf sock buf (fromIntegral r)
                    ptyToSock' buf
              -- putStr $ cnv a
        Left (e :: IOException) -> handleE e
        where
          handleE e -- handle pty slave termination gracefully
            | ioeGetErrorType e == HardwareFault = pure ()
            | otherwise                          = throwIO e


sendAllBuf :: Socket -> Ptr Word8 -> Int -> IO ()
sendAllBuf sock buf c = do
  c' <- sendBuf sock buf c
  case min 0 $ c - c' of
    0 -> pure ()
    r -> sendAllBuf sock (plusPtr buf c') r


