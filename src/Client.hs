module Client where

import           Protolude

import qualified Message                   as Msg

import           Network.Socket
import           Network.Socket.ByteString (sendAll)

import qualified Data.Text.Encoding        as T
import           Pty                       (PtyData (..))

import           Message                   (CryptoCtx (..), NetworkBytes (..),
                                            ParseCtx (..))


data ClientCtx = ClientCtx { messageCtx :: Msg.Ctx }


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
talk Msg.Ctx{readCryptoCtx, writeCryptoCtx, parseCtx} sock = do
  withAsync (sockToCons sock (parseCtx, readCryptoCtx)) $ \a1 -> do
    withAsync (consToSock sock writeCryptoCtx) $ \a2 -> do
      void $ waitAnyCancel [a1, a2]
      putText "Finished"

-- FIXME: change it to transport each keystroke
-- possibly echo from the other party?
consToSock :: Socket -> CryptoCtx -> IO ()
consToSock sock cctx = do
  d <- PtyData . T.encodeUtf8 . (<> "\n") <$> getLine
  let (nbs, cctx') =
        first (Msg.combine . (:[]) . Msg.serialize) (Msg.mkMessage d cctx)
  sendAll sock $ unNetworkBytes nbs
  consToSock sock cctx'


-- probably we should write directly to console buffer...
sockToCons :: Socket -> (ParseCtx, CryptoCtx) -> IO ()
sockToCons sock (pctx, cctx) = do
  nbs <- fmap (NetworkBytes . toS) $ recv sock 1024
  let (nms'e, pctx') = Msg.tokenize nbs pctx
  case fmap (first reverse) $ foldM f ([], cctx) $ rights nms'e of
    Right ([], cctx') -> sockToCons sock (pctx', cctx')
    Right (xs, cctx') -> do mapM_ (putStr . unpack . unPtyData) xs
                            sockToCons sock (pctx', cctx')
    Left e -> do putText $ "Fatal error: " <> e
  where
    f (acc, ctx) x = do
      (d, ctx') <- Msg.deserialize x >>= flip Msg.readMessage ctx
      pure (d:acc, ctx')
    unpack :: ByteString -> Text
    unpack bs = case T.decodeUtf8' bs of
      Right v -> v
      Left _  -> "<???>"

