module Boot.Client where

import           Protolude
import           Protolude.Extended

import           Network.Socket            hiding (recv, send)
import           Network.Socket.ByteString
import           System.Timeout            (timeout)

import           Boot.Transport


type Seconds = Int -- FIXME newtype

data Config = Config { address      :: Text
                     , port         :: Int
                     , replyTimeout :: Seconds
                     } deriving (Show)

defaultConfig :: Config
defaultConfig = Config "127.0.0.1" 3000 30


client :: Config -> IO ()
client cfg = do
  putText $ "Running client"
  runExceptT (process cfg) >>= \case
    Right () -> putText "Ok"
    Left e -> putText $ "Error: " <> e

process :: Config -> ExceptT ErrText IO ()
process Config{address, port, replyTimeout} = do
  bracketE (liftIO $ openSocket address port) (liftIO . close) $ \sock -> do
    sendHello sock $ Hello HelloC Nothing
    hr <- recvHelloResp sock replyTimeout
    liftIO $ putText $ "Received: " <> show hr

sendHello :: Socket -> Hello -> ExceptT ErrText IO ()
sendHello sock msg = handleIOEx (sendAll sock $ toBS msg)

recvHelloResp :: Socket -> Seconds -> ExceptT ErrText IO HelloResp
recvHelloResp sock to' = do
  handleIOEx (timeout (1000 * 1000 * to') $ recv sock 4096) >>= \case
    Just bs -> liftEither $ fromBS bs
    Nothing -> throwError "Timeout waiting for HelloResp"


openSocket :: Text -> Int -> IO Socket
openSocket a' p' = do
  putText $ "Opening UDP socket for " <> a' <> ":" <> show p'
  addrinfos <- getAddrInfo Nothing (Just $ toS a') (Just $ show p')
  case head addrinfos of
    Just addr -> do
      bracketOnError (socket (addrFamily addr) Datagram defaultProtocol)
                     (close) $ \sock ->
                     do connect sock (addrAddress addr)
                        pure sock
    Nothing -> panic "No addresses found"

