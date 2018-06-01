{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import Protolude

import System.Posix.Types (CSsize(..), Fd(..))
import System.Posix.IO (fdReadBuf)
import System.Process.Internals (mkProcessHandle, ProcessHandle)
import System.IO.Error (ioeGetErrorType)

import Foreign
import Foreign.C.Types
import Foreign.C.Error (throwErrnoIfMinus1Retry)

import Foreign.Marshal.Array (allocaArray, peekArray)
--import Data.Typeable

import GHC.IO.Exception (IOErrorType (HardwareFault))


type CIntPty = CInt
type CIntPid = CInt

-- type Pid = Int -- CPid

-- let's be honest as to parameter/return types
foreign import ccall "fork_exec_with_pty.h fork_exec_with_pty"
     c_fork_exec_with_pty :: Ptr CIntPty -> IO CIntPid


fork_exec_with_pty :: IO (Fd, ProcessHandle)
fork_exec_with_pty = do
  alloca $ \ptyPtr -> do
    pid <- throwErrnoIfMinus1Retry "failed to fork or open pty" $
          c_fork_exec_with_pty ptyPtr
    pty <- peek ptyPtr
    ph <- mkProcessHandle (undefined pid) True
    pure (Fd pty, ph)

go :: IO ()
go = do
  (fd, _) <- fork_exec_with_pty
  putText $ "got pty: " <> show fd
  readMe fd
  putText "Finished"

bs :: Int
bs = 1024

readMe :: Fd -> IO ()
readMe fd = do
  allocaArray bs $ \buf -> do
    putText "Before reading"
    r'e <- try (fdReadBuf fd buf $ fromIntegral bs)
    case r'e of
      Right r -> do 
        putText $ "Read: " <> show r
        a <- peekArray (fromIntegral r) buf
        case r of
          0 -> pure () -- putText "*** EOF ***"
          _ -> do putText $ cnv a
                  readMe fd
      Left (e :: IOException) -> handleE e
  where
    handleE e -- handle graceful exit on pty slave termination 
      | ioeGetErrorType e == HardwareFault = pure () 
      | otherwise                          = throwIO e

cnv :: [Word8] -> Text
cnv ws = toS $ map (chr . fromIntegral) ws

foreign import ccall safe "read"
   c_safe_read :: CInt -> Ptr CChar -> CSize -> IO CSsize
