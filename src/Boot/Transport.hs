module Boot.Transport where

import           Protolude
import           Protolude.Extended

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base64           as BS
import qualified Data.Text.Encoding               as T

import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as AC (decimal, signed)

import           Control.Monad                    (fail)


class ToBS a where
  toBS :: a -> ByteString

class FromBS a where
  fromBS :: ByteString -> Either ErrText a


data Command = HelloC deriving (Eq, Show)

instance ToBS Command where
  toBS HelloC = "Haskell-rocks!"

instance FromBS Command where
  fromBS "Haskell-rocks!" = pure HelloC
  fromBS _                = Left "Invalid command"


data Hello = Hello { command  :: !Command
                   , offset'm :: !(Maybe Int)
                   } deriving (Eq, Show)

instance ToBS Hello where
  toBS (Hello cmd (Just offset)) = toBS cmd <> ":" <> show offset
  toBS (Hello cmd Nothing)       = toBS cmd

instance FromBS Hello where
  fromBS xs = mapLeft toS $ A.parseOnly (helloP <* A.endOfInput) xs
    where
      helloP = do
        cmd <- fromBS <$> A.takeWhile (/= col) >>= \case
          Right v -> pure v
          Left e -> fail $ toS e
        A.peekWord8 >>= \case
          Just _ -> Hello <$> pure cmd
                          <*> fmap Just (A.word8 col *> AC.signed AC.decimal)
          Nothing -> pure $ Hello cmd Nothing


data HelloResp = HelloOk { iv :: !ByteString }
               | HelloFail { err :: !Text }
               deriving (Eq, Show)


instance ToBS HelloResp where
  toBS (HelloOk iv)    = "ok" <> ":" <> BS.encode iv
  toBS (HelloFail msg) = "err" <> ":" <> T.encodeUtf8 msg

instance FromBS HelloResp where
  fromBS bs = do mapLeft toS $ A.parseOnly (p <* A.endOfInput) bs
    where
      p = A.takeWhile (/= col) <* A.word8 col >>= \case
        "ok" -> p1
        "err" -> p2
        _ -> fail "Unknown response type"
      p1 = BS.decode . BS.pack <$> A.many1 A.anyWord8 >>= \case
        Right xs -> pure $ HelloOk xs
        Left _ -> fail "Invalid iv encoding"
      p2 = decodeUtf8' . BS.pack <$> A.many1 A.anyWord8 >>= \case
        Right xs -> pure $ HelloFail xs
        Left _ -> fail "Invalid error message"

col :: Word8
col = fromIntegral $ ord ':'
