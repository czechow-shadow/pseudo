module Message
  ( NetworkBytes (..)
  , SecMessage (..)
  , NetworkMessage (..)
  , Iv (..)
  , Secret (..)
  , ParseCtx (..)
  , CryptoCtx (..)
  , Ctx (..)
  , mkCtx
  , tokenizeM
  , tokenize
  , mkCryptoCtx
  , emptyParseCtx
  , stx
  , etx
  , serialize
  , deserialize
  , combine
  , mkMessage
  , readMessage
  ) where

import           Protolude
import           Protolude.Extended

import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as BS64
import qualified Data.ByteString.Char8      as BSC

import qualified Data.Text.Encoding         as T
import qualified Data.Text.Read             as T

import           Control.Monad              (fail)

import qualified Crypto.Cipher              as C
import           Crypto.Cipher.Types        (AuthTag (..))
import qualified Crypto.Cipher.Types        as C

import           Text.Printf                (printf)

import           Pty


newtype EncPtyData = EncPtyData { unEncPtyData :: ByteString }
                   deriving (Eq, Show)

newtype SeqNum = SeqNum { unSeqNum :: Int }
               deriving (Eq, Show)

newtype SeqNumFmt = SeqNumFmt { unSeqNumFmt :: ByteString }
                  deriving (Eq, Show)


data SecMessage = SecMessage { seqNumFmt :: SeqNumFmt
                             , authTag   :: AuthTag
                             , ptyData   :: EncPtyData
                             } deriving (Eq, Show)

newtype NetworkBytes = NetworkBytes { unNetworkBytes :: ByteString }
                     deriving (Eq, Show)

newtype NetworkMessage = NetworkMessage { unNetworkMessage :: ByteString }
                         deriving (Eq, Show)


data ParsePhase = PPIn | PPOut deriving (Eq, Show)
type LeftOvers = NetworkBytes

data ParseCtx = ParseCtx ParsePhase LeftOvers
              deriving (Eq, Show)


newtype Iv = Iv { unIv :: ByteString } deriving Show
newtype Secret = Secret { unSecret :: ByteString } deriving Show

data CryptoCtx = CryptoCtx { seqNum :: SeqNum
                           , aead   :: C.AEAD C.AES256
                           }

data Ctx = Ctx { readCryptoCtx  :: CryptoCtx
               , writeCryptoCtx :: CryptoCtx
               , parseCtx       :: ParseCtx
               }

stx :: Word8
stx = fromIntegral $ ord '\STX'

etx :: Word8
etx = fromIntegral $ ord '\ETX'

maxMsgLen :: Int
maxMsgLen = 8192

authTagLen :: Int
authTagLen = 16

msgCounterLen :: Int
msgCounterLen = 4


-------------------------------------------------------------------------------
--                            PtyData to network
-------------------------------------------------------------------------------
mkMessage :: PtyData -> CryptoCtx -> (SecMessage, CryptoCtx)
mkMessage (PtyData d) CryptoCtx{seqNum, aead} =
  let aead' = C.aeadAppendHeader aead snf
      (ed, aead'') = C.aeadEncrypt aead' d
      at = C.aeadFinalize aead'' authTagLen
      msg = SecMessage { seqNumFmt = SeqNumFmt snf
                       , authTag = at
                       , ptyData = EncPtyData ed
                       }
      ctx' = CryptoCtx { seqNum = nextSeqNum seqNum, aead = aead'' }
  in (msg, ctx')
  where
    snf = fmtNum msgCounterLen $ unSeqNum seqNum

serialize :: SecMessage -> NetworkMessage
serialize SecMessage {seqNumFmt, authTag = AuthTag at, ptyData} =
  NetworkMessage $
     BS.singleton stx
  <> unSeqNumFmt seqNumFmt
  <> BS64.encode (at <> unEncPtyData ptyData)
  <> BS.singleton etx

combine :: [NetworkMessage] -> NetworkBytes
combine = NetworkBytes . BS.concat . map unNetworkMessage

-------------------------------------------------------------------------------
--                           network to PtyData
-------------------------------------------------------------------------------
-- FIXME: transformers here???
tokenizeM :: (MonadState ParseCtx m) =>
             NetworkBytes -> m [Either ErrText NetworkMessage]
tokenizeM xs = do
  ctx <- get
  let (res, ctx') = tokenize xs ctx
  put ctx'
  return res

tokenize :: NetworkBytes -> ParseCtx
         -> ([Either ErrText NetworkMessage], ParseCtx)
tokenize (NetworkBytes buf) pctx@(ParseCtx _ (NetworkBytes leftOvers))
 | BS.length buf + BS.length leftOvers > maxMsgLen =
   ( [Left "SecMessage too long, contents dropped"]
   , ParseCtx PPOut $ NetworkBytes ""
   )
 | otherwise = tokenize' (NetworkBytes buf) pctx

tokenize' :: NetworkBytes -> ParseCtx
          -> ([Either ErrText NetworkMessage], ParseCtx)
tokenize' (NetworkBytes buf) (ParseCtx pp (NetworkBytes lo)) =
  case pp of
    PPOut -> case BS.span (/= stx) (lo <> buf) of
      ("", ys) -> tokenize' (NetworkBytes ys) (ParseCtx PPIn $ NetworkBytes "")
      (xs, "") -> ([Left $ garbage xs], ParseCtx pp $ NetworkBytes "")
      (xs, ys) -> first ((Left $ garbage xs):) $
                        tokenize' (NetworkBytes ys)
                                   (ParseCtx PPIn $ NetworkBytes "")
    PPIn -> case BS.span (/= etx) (lo <> buf) of
      (xs, "") -> ([], ParseCtx PPOut $ NetworkBytes xs)
      (xs, ys) -> first ((Right $ NetworkMessage $ xs <> BS.take 1 ys):) $
                        tokenize' (NetworkBytes $ BS.drop 1 ys)
                                   (ParseCtx PPOut $ NetworkBytes "")
  where
    garbage xs = "Dropped garbage of length " <> (show $ BS.length xs)


deserialize :: NetworkMessage -> Either ErrText SecMessage
deserialize (NetworkMessage xs) = do
  checkLen
  mapLeft show $ P.eitherResult $ P.parse msgP xs
  where
    checkLen | BS.length xs > maxMsgLen = Left "Message length exceeded"
             | otherwise = pure ()
    msgP = do
      _ <- P.word8 stx
      sn <- P.take msgCounterLen
      (at, bs) <- fmap (BS.splitAt authTagLen) $
                       P.takeWhile (/= etx) >>= rdBase64
      _ <- P.word8 etx
      pure SecMessage { seqNumFmt = SeqNumFmt sn
                      , authTag = AuthTag at
                      , ptyData = EncPtyData bs
                      }
    rdBase64 :: ByteString -> P.Parser ByteString
    rdBase64 ys = case BS64.decode ys of
      Right v -> pure v
      Left e  -> fail e


readMessage :: SecMessage -> CryptoCtx -> Either ErrText (PtyData, CryptoCtx)
readMessage SecMessage{seqNumFmt, authTag, ptyData}
            CryptoCtx{seqNum, aead} = do
  checkSn (unSeqNumFmt seqNumFmt)
  let aead' = C.aeadAppendHeader aead $ unSeqNumFmt seqNumFmt
      (dd, aead'') = C.aeadDecrypt aead' $ unEncPtyData ptyData
      at = C.aeadFinalize aead'' authTagLen

  checkAt at
  pure (PtyData dd, CryptoCtx { seqNum = nextSeqNum seqNum, aead = aead'' })
  where
    checkAt at | at == authTag = pure ()
               | otherwise = Left $ "Incorrect AuthTag"
    checkSn sn = do
      d <- fst <$> rdDecimal sn
      if d == unSeqNum seqNum
        then pure ()
        else Left $ "Incorrect packet seqNum. Expected "
                 <> show (unSeqNum seqNum) <> ", received " <> show d
    rdDecimal ys = mapLeft show $ T.decimal $ T.decodeUtf8 ys

-------------------------------------------------------------------------------
--                             Initializations
-------------------------------------------------------------------------------
mkCtx :: (Iv, Secret) -> (Iv, Secret) -> Either ErrText Ctx
mkCtx (iv, sec) (iv', sec') =
  Ctx <$> mkCryptoCtx iv sec
      <*> mkCryptoCtx iv' sec'
      <*> pure emptyParseCtx

mkCryptoCtx :: Iv -> Secret -> Either ErrText CryptoCtx
mkCryptoCtx iv sec = CryptoCtx <$> pure (SeqNum 5678) <*> mkAEAD iv sec

emptyParseCtx :: ParseCtx
emptyParseCtx = ParseCtx PPOut (NetworkBytes "")

mkAEAD :: Iv -> Secret -> Either ErrText (C.AEAD C.AES256)
mkAEAD iv secret = do
   iv' <- maybeToEither "InvalidIV" $ C.makeIV $ unIv iv
   k <- mapLeft show $ C.makeKey $ unSecret secret
   maybeToEither "Failed to initialize AEAD" $
        C.aeadInit C.AEAD_GCM (C.cipherInit k) (iv' :: C.IV C.AES256)

-------------------------------------------------------------------------------
--                             Helper functions
-------------------------------------------------------------------------------
nextSeqNum :: SeqNum -> SeqNum
nextSeqNum (SeqNum v) = SeqNum $ succ v `mod` (10 :: Int)^msgCounterLen

fmtNum :: Int -> Int -> ByteString
fmtNum l = BSC.pack . printf "%0*d" l




