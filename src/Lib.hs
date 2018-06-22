
module Lib where

import           Protolude

import           System.Timeout (timeout)

import qualified Message        as Msg

import           Client
import           Server


iv :: Msg.Iv
iv = Msg.Iv "2234567890123456" -- FIXME: generate & send over base64

sec :: Msg.Secret
sec = Msg.Secret "12345678901234561234567890123456"


go :: IO ()
go = do
  case Msg.mkCtx (iv, sec) (iv, sec) of
    Right ctx -> go' $ ServerCtx ctx
    Left e    -> putText $ "Error: " <> e
  where
  go' ctx = do
    putText "Listening"
    void $ timeout (1000 * 1000 * 5000) $
           bracket openSocket closeSocket $ acceptLoop ctx
    putText "Sleeping"
    threadDelay $ 1000 * 1000 * 5
    go


goclient :: IO ()
goclient = do
  case Msg.mkCtx (iv, sec) (iv, sec) of
    Right ctx -> client $ ClientCtx ctx
    Left e    -> putText $ "Error: " <> e


