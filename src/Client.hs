module Client where

import           Protolude

import           Network.Socket

import           Pipes
import           Pipes.ByteString  (hGetSome, toHandle)
import           Pipes.Network.TCP (fromSocket, toSocket)
import qualified Pipes.Prelude     as P

import           Message           (CryptoCtx (..), NetworkBytes (..))
import qualified Message           as Msg
import           Pty               (PtyData (..))


data ClientCtx = ClientCtx { messageCtx :: Msg.Ctx }

bsize :: Int
bsize = 4096


client :: ClientCtx -> IO ()
client ClientCtx{messageCtx} = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  setSocketOption sock ReusePort 1

  let addr = SockAddrInet 3000 (tupleToHostAddress (127,0,0,1))
  connect sock addr

  putText $ "Opening connection to " <> show addr
  talk messageCtx sock

talk :: Msg.Ctx -> Socket -> IO ()
talk Msg.Ctx{readCryptoCtx, writeCryptoCtx} sock = do
  withAsync (sockToCons stdout sock readCryptoCtx) $ \a1 -> do
    withAsync (consToSock stdin sock writeCryptoCtx) $ \a2 -> do
      void $ waitAnyCancel [a1, a2]
      putText "Finished"

consToSock :: Handle -> Socket -> CryptoCtx -> IO ()
consToSock h sock cctx = do
  runEffect $ hGetSome bsize h >-> P.map PtyData
          >-> Msg.toTransport cctx
          >-> P.map unNetworkBytes >-> toSocket sock

sockToCons :: Handle -> Socket -> CryptoCtx -> IO ()
sockToCons h sock cctx = do
  runEffect $ fromSocket sock bsize >-> P.map NetworkBytes
          >-> Msg.fromTransport cctx
          >-> P.map unPtyData >-> toHandle h
