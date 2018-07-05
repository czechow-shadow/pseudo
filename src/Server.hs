module Server where

import           Protolude

import           Foreign.C.Types
import           Network.Socket
import           System.Posix.Types (Fd (..))

import           System.Posix.IO    (closeFd, fdToHandle)

import           Pipes
import           Pipes.ByteString   (hGetSome, toHandle)
import           Pipes.Network.TCP  (fromSocket, toSocket)
import qualified Pipes.Prelude      as P

import           Message            (NetworkBytes (..))
import qualified Message            as Msg
import           Pty                (PtyData (..))
import qualified Pty


data ServerCtx = ServerCtx { messageCtx :: Msg.Ctx }

type CIntPty = CInt
type CIntPid = CInt


-- Socket and pty operations buffer size
bsize :: Int
bsize = 4096

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
acceptLoop :: ServerCtx -> (Msg.Ctx -> Socket -> IO ()) -> Socket -> IO ()
acceptLoop ServerCtx{messageCtx} action sock = forever $ do
  (sock', peer) <- accept sock
  -- allow connections to die fast...
  setSocketOption sock' KeepAlive 1
  setSocketOption sock' UserTimeout 30
  setSocketOption sock' (CustomSockOpt (ipProtoTcp, tcpKeepIdle)) 3
  setSocketOption sock' (CustomSockOpt (ipProtoTcp, tcpKeepIntvl)) 1
  -- FIXME: this one gets ignored :-(
  setSocketOption sock' (CustomSockOpt (ipProtoTcp, tcpKeepCnt)) 3
  putText $ "Connection from " <> show peer
  async $ action messageCtx sock' `finally` close sock'

talk :: Msg.Ctx -> Socket -> IO ()
talk Msg.Ctx{readCryptoCtx, writeCryptoCtx} sock = do
  putText $ "Spawning bash and talking to it"
  bracket allocPty closePty $ \(fd, _) ->
    withAsync (sockToPty readCryptoCtx sock fd) $ \as1 ->
    withAsync (ptyToSock writeCryptoCtx sock fd) $ \as2 -> do
    void $ waitAny [as1, as2]
    putText $ "Talk finished"
  where
    allocPty = Pty.forkExecWithPty
    closePty (fd, _) = closeFd fd
    -- FIXME: we should close ph too...
    -- or have some sort of ping messages to detect other party's closed socket
    -- closePty (fd, ph) = Pty.close (fd, ph)

sockToPty :: Msg.CryptoCtx -> Socket -> Fd -> IO ()
sockToPty cctx sock fd = do
  h <- liftIO $ fdToHandle fd
  runEffect $ fromSocket sock bsize >-> P.map NetworkBytes
          >-> Msg.fromTransport cctx
          >-> P.map unPtyData >-> toHandle h

ptyToSock :: Msg.CryptoCtx -> Socket -> Fd -> IO ()
ptyToSock cctx sock fd = do
  h <- liftIO $ fdToHandle fd
  runEffect $ hGetSome bsize h >-> P.map PtyData
          >-> Msg.toTransport cctx
          >-> P.map unNetworkBytes >-> toSocket sock
