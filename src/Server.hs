module Server where

import           Protolude

import qualified Data.ByteString       as BS
import           Foreign               hiding (void)
import           Foreign.C.Types
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Network.Socket
import           System.Posix.Types    (Fd (..))

import           System.Posix.IO       (closeFd)

import           Message               (NetworkBytes (..))
import qualified Message               as Msg
import qualified Pty


data ServerCtx = ServerCtx { messageCtx :: Msg.Ctx }

type CIntPty = CInt
type CIntPid = CInt


-- Socket buffer size
bsize :: Int
bsize = 1024

tcpKeepIdle :: CInt
tcpKeepIdle = 4

tcpKeepIntvl :: CInt
tcpKeepIntvl = 5

tcpKeepCnt :: CInt
tcpKeepCnt = 6

ipProtoTcp :: CInt
ipProtoTcp = 6


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
acceptLoop :: ServerCtx -> Socket -> IO ()
acceptLoop ServerCtx{messageCtx} sock = forever $ do
  (sock', peer) <- accept sock
  -- allow connections to die fast...
  setSocketOption sock' KeepAlive 1
  setSocketOption sock' UserTimeout 30
  setSocketOption sock' (CustomSockOpt (ipProtoTcp, tcpKeepIdle)) 3
  setSocketOption sock' (CustomSockOpt (ipProtoTcp, tcpKeepIntvl)) 1
  -- FIXME: this one gets ignored :-(
  setSocketOption sock' (CustomSockOpt (ipProtoTcp, tcpKeepCnt)) 3
  putText $ "Connection from " <> show peer
  async $ talk messageCtx sock' `finally` close sock'

talk :: Msg.Ctx -> Socket -> IO ()
talk Msg.Ctx{parseCtx, readCryptoCtx, writeCryptoCtx} sock = do
  putText $ "Spawning bash and talking to it"
  bracket allocPty closePty $ \(fd, _) ->
    withAsync (sockToPty (parseCtx, readCryptoCtx) sock fd) $ \as1 -> do
    withAsync (ptyToSock writeCryptoCtx sock fd) $ \as2 -> do
      void $ waitAny [as1, as2]
  putText $ "Talk finished"
  where
    allocPty = Pty.forkExecWithPty
    closePty (fd, _) = closeFd fd


-- FIXME: socket close will cause Exception, I think
sockToPty :: (Msg.ParseCtx, Msg.CryptoCtx) -> Socket -> Fd -> IO ()
sockToPty (_pctx, _cctx) sock fd =
  allocaArray bsize $ \buf -> sockToPty' buf _pctx _cctx
  where
    sockToPty' buf pctx cctx = do
      -- FIXME: shall we wrap it in an exception?
      recvBuf sock buf (fromIntegral bsize) >>= \case
        0 -> putText "Peer closed connection"
        c -> do nbs <- NetworkBytes . BS.pack <$> peekArray c buf
                let (nms'e, pctx') = Msg.tokenize nbs pctx
                mapM_ (\v -> putText $ "WARN: " <> v) $ lefts nms'e
                case fmap (first reverse) $ foldM f ([], cctx) $ rights nms'e of
                  Right ([], cctx') -> sockToPty' buf pctx' cctx'
                  Right (xs, cctx') -> write fd xs >>= \case
                    Just () -> sockToPty' buf pctx' cctx'
                    Nothing -> do putText "Pty EOF"
                                  close sock
                  Left e -> putText $ "Fatal error: " <> e

    f (acc, ctx) x = do
      (d, ctx') <- Msg.deserialize x >>= flip Msg.readMessage ctx
      pure (d:acc, ctx')

    write _ [] = pure $ Just ()
    write fd' (y:ys) = Pty.write y fd' >>= \case
      Just () -> write fd' ys
      Nothing -> pure Nothing


ptyToSock :: Msg.CryptoCtx -> Socket -> Fd -> IO ()
ptyToSock _cctx sock fd = ptyToSock' _cctx
  where
    ptyToSock' cctx = Pty.read fd >>= \case
      Just d -> do
        let (smsg, cctx') = Msg.mkMessage d cctx
            nbs = Msg.combine [Msg.serialize smsg]
        BS.useAsCStringLen (unNetworkBytes nbs) $ \(buf, len) ->
          sendAllBuf sock (castPtr buf) len
        ptyToSock' cctx'
      Nothing -> do putText "EOF"

sendAllBuf :: Socket -> Ptr Word8 -> Int -> IO ()
sendAllBuf sock buf c = do
  c' <- sendBuf sock buf c
  case min 0 $ c - c' of
    0 -> pure ()
    r -> sendAllBuf sock (plusPtr buf c') r






