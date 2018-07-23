
module Boot.Server where

import           Protolude
import           Protolude.Extended

import           Network.Socket            hiding (recv, recvFrom, send,
                                            sendTo)
import           Network.Socket.ByteString
import           System.Timeout            (timeout)

import           Boot.Transport


server :: Config -> IO ()
server Config {address, port, listenDuration, sleepDuration} = do
  mv <- newEmptyMVar
  go' mv
  where
    go' mv = do
      putText $ "Listening for " <> show listenDuration <> "s"
      void $ timeout (s2us listenDuration) $
             bracket (openSocket address port) closeSocket (serveConns mv)
      putText $ "Sleeping for " <> show sleepDuration <> "s"
      threadDelay $ s2us 5
      go' mv

type Seconds = Int -- FIXME newtype

data Config = Config { address        :: Text
                     , port           :: Int
                     , listenDuration :: Seconds
                     , sleepDuration  :: Seconds
                     } deriving (Show)


defaultConfig :: Config
defaultConfig = Config "0.0.0.0" 3000 10 5


openSocket :: Text -> Int -> IO Socket
openSocket a' p' = do
  putText $ "Opening UDP socket for " <> a' <> ":" <> show p'
  addrinfos <- getAddrInfo Nothing (Just $ toS a') (Just $ show p')
  case head addrinfos of
    Just addr -> do
      bracketOnError (socket (addrFamily addr) Datagram defaultProtocol)
                     (close) $ \sock ->
                     do bind sock (addrAddress addr)
                        pure sock
    Nothing -> panic "No addresses found"

closeSocket :: Socket -> IO ()
closeSocket sock = do close sock
                      putText "Closed UDP socket"


serveConns :: MVar () -> Socket -> IO ()
serveConns mv sock = do
  runExceptT serve >>= \case
    Right () -> serveConns mv sock
    Left e -> do putText $ "Error serving connection: " <> e
                 serveConns mv sock
  serveConns mv sock
    where
      serve :: ExceptT ErrText IO ()
      serve = do
        (p, sa) <- receive
        as'm <- liftIO $ mask $ \restore -> do
          tryPutMVar mv () >>= \case
            True -> do as <- restore $ do iv <- pure "sdfsdf"
                                          async $ spawn p iv `finally` takeMVar mv
                       pure $ Just as
            False -> pure Nothing

        case as'm of
          Just as -> do iv <- pure "sdfsdf"
                        reply sa (HelloOk iv) `onExceptionE` (liftIO $ cancel as)
          Nothing -> reply sa $ HelloFail "Already running"

      receive :: ExceptT ErrText IO (Int, SockAddr)
      receive = do
        (r, sa) <- handleIOEx (recvFrom sock 4096)
        case fromBS r of
          Right (Hello HelloC po'm) -> pure (maybe 0 identity po'm, sa)
          Left e -> throwError $ "Error reading input packet: " <> e

      spawn :: Int -> ByteString -> IO ()
      spawn portOffset _ = run
        where
          run = do -- putText $ "Running server with portOffset " <> show portOffset
                   threadDelay $ s2us 3
                   pure ()

      reply :: SockAddr -> HelloResp -> ExceptT ErrText IO ()
      reply sa resp = handleIOEx (void $ sendTo sock (toBS resp) sa)

s2us :: Int -> Int
s2us = (* 1000000)

-- FIXME: remove all below
go3 :: IO ()
go3 = do
  op
  rn `onException` cl
    where
      op = putText "Alloc"
      cl = putText "Dealloc"
      rn = do putText "Running"
              threadDelay $ s2us 30

go4 :: IO ()
go4 = do
  void $ runExceptT go4'




go4' :: ExceptT ErrText IO ()
go4' = bracketOnErrorE op (const cl) (const rn)
    where
      op :: ExceptT ErrText IO ()
      op = putText "Alloc"

      cl :: ExceptT ErrText IO ()
      cl = putText "Dealloc"

      rn :: ExceptT ErrText IO ()
      rn = do putText "Running"
     --         throwError "Throwing pcz"
              liftIO $ threadDelay $ s2us 10

