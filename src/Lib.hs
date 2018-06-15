
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

import qualified Data.Text                as T
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
go = go3
-- go = do
--   putText "Listening"
--   void $ timeout (1000 * 1000 * 5) $ bracket openSocket closeSocket acceptLoop
--   putText "Sleeping"
--   threadDelay $ 1000 * 1000 * 5
--   go


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

-- FIXME: res leak if killed between accept and async...
acceptLoop :: Socket -> IO ()
acceptLoop sock = forever $ do
  (sock', peer) <- accept sock
  putText $ "Connection from " <> show peer
  async $ talk sock' `finally` close sock'

talk :: Socket -> IO ()
talk sock = do
  putText $ "Spawning bash and talking to it"
  bracket allocPty closePty $ \(fd, ph) ->
    withAsync (sockToPty sock fd) $ \as1 -> do
    withAsync (ptyToSock sock fd) $ \as2 -> do
      void $ waitAny [as1, as2]
      void $ waitForProcess ph
  putText $ "Talk finished"
  where
    allocPty = fork_exec_with_pty
    closePty = closeFd . fst

-- FIXME: socket close will cause Exception, I think
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

type LeftOvers = Text -- ByteString

data ParsePhase = PPIn | PPOut deriving Show


data CryptCtx = CryptCtx
data ParseCtx = ParseCtx ParsePhase LeftOvers deriving Show

empytParseCtx :: ParseCtx
empytParseCtx = ParseCtx PPOut ""

data Message = Message Text deriving Show
type ErrText = Text


-- go3 :: IO ()
-- go3 = runStateT go3' (ParseCtx PPOut "") >>= putText . show

go3 :: IO ()
go3 = go' empytParseCtx
  where
    go' pctx = receive0 >>= \case
      Just xs -> do let (res, pctx') = receive' xs pctx
                    putText $ show res
                    go' pctx'
      Nothing -> putText "Eof"


go3' :: (MonadIO m, MonadState ParseCtx m) => m [Either ErrText Message]
go3' = receive0 >>= \case
  Just xs -> (++) <$> receiveM xs <*> go3'
  Nothing -> pure []


receive0 :: (MonadIO m) => m (Maybe Text)
receive0 = do
  putText "get line> "
  liftIO $ try (liftIO getLine) >>= \case
    Right x -> pure $ Just x
    Left (_::IOException) -> pure Nothing

receiveM :: (MonadState ParseCtx m) => Text -> m [Either ErrText Message]
receiveM xs = do
  ctx <- get
  let (res, ctx') = receive' xs ctx
  put ctx'
  pure res

receive' :: Text -> ParseCtx -> ([Either ErrText Message], ParseCtx)
receive' buf pctx@(ParseCtx _ leftOvers)
 | T.length buf + T.length leftOvers > 8192 =
   ([Left "Message too long, contents dropped"], ParseCtx PPOut "")
 | otherwise = receive'' buf pctx

receive'' :: Text -> ParseCtx -> ([Either ErrText Message], ParseCtx)
receive'' buf (ParseCtx pp lo) = case pp of
  PPOut -> case T.span (/= '<') (lo <> buf) of
    ("", ys) -> receive'' ys (ParseCtx PPIn "")
    (xs, "") -> ([Left $ garbage xs], ParseCtx pp "")
    (xs, ys) -> first ((Left $ garbage xs):) $
                      receive'' ys (ParseCtx PPIn "")
  PPIn -> case T.span (/= '>') (lo <> buf) of
    (xs, "") -> ([], ParseCtx pp xs)
    (xs, ys) -> first ((Right $ Message $ xs <> T.take 1 ys):) $
                      receive'' (T.drop 1 ys) (ParseCtx PPOut "")
  where
    garbage xs = "Dropped garbage: [" <> xs <> "]"




recvMessage :: Socket -> IO ()
recvMessage = undefined

-- do we rely on exceptions?
recvEncrypted :: Socket -> Ptr Word8 -> StateT (CryptCtx, ParseCtx) IO ()
recvEncrypted sock buf = do
  (cCtx, pCtx) <- get
  c <- liftIO $ recvBuf sock buf (fromIntegral bs) -- FIXME: handle exception

  undefined


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


