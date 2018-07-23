{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib where

import           Protolude       hiding (pi)

import           System.IO       (hSetEcho)
import           System.Timeout  (timeout)

import qualified Data.ByteString as BS

import qualified Message         as Msg

import           Client
import           Server


iv :: Msg.Iv
iv = Msg.Iv "2234567890123456" -- FIXME: generate & send over base64

otherIv :: Msg.Iv -> Msg.Iv
otherIv (Msg.Iv bs) = Msg.Iv $ BS.map (xor 0xff) bs


sec :: Msg.Secret
sec = Msg.Secret "12345678901234561234567890123456"


go :: IO ()
go = do
  case Msg.mkCtx (iv, sec) (otherIv iv, sec) of
    Right ctx -> go' $ ServerCtx ctx
    Left e    -> putText $ "Error: " <> e
  where
    go' ctx = do
      putText "Listening"
      void $ timeout (1000 * 1000 * 5000) $
             bracket openSocket closeSocket $ acceptLoop ctx Server.talk
      putText "Sleeping"
      threadDelay $ 1000 * 1000 * 5
      go


goclient :: IO ()
goclient = do
  case Msg.mkCtx (otherIv iv, sec) (iv, sec) of
    Right ctx -> do
      hSetEcho stdout False
      client (ClientCtx ctx) `finally` hSetEcho stdout True
    Left e    -> putText $ "Error: " <> e

go3 :: IO ()
go3 = do
  mv <- newEmptyMVar
  go3' mv
  where
    go3' mv = do
      putStr ("> " :: Text)
      getLine >>= \case
        "take" -> do
          r <- tryPutMVar mv ()
          if r
            then void $ async $ do putText ">> Running server"
                                   (do threadDelay $ 1000 * 1000 * 10
                                       threadDelay $ 10
                                     ) `finally` (do takeMVar mv
                                                     putText ">> Stopped server")
            else do putText ">> Server already running"
          go3' mv
        "quit" -> pure ()
        xs -> do putText $ "Ignoring " <> xs
                 go3' mv


