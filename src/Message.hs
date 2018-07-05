module Message
  ( NetworkBytes (..)
  , SecMessage (..)
  , EncPtyData (..)
  , Iv (..)
  , Secret (..)
  , ParseC
  , CryptoCtx (..)
  , Ctx (..)
  , SeqNumFmt (..)
  , mkParseC
  , mkCtx
  , mkCryptoCtx
  , stx
  , etx
  , serialize
  , deserialize -- FIXME: change name
  , mkMessage
  , readMessage
  , toTransport
  , fromTransport
  ) where

import           Protolude
import           Protolude.Extended

import qualified Data.Attoparsec.ByteString as A
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

import           Pipes

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

type ParseC a = ByteString -> A.Result (Either ErrText a)


newtype Iv = Iv { unIv :: ByteString } deriving Show
newtype Secret = Secret { unSecret :: ByteString } deriving Show

data CryptoCtx = CryptoCtx { seqNum :: SeqNum
                           , aead   :: C.AEAD C.AES256
                           }

data Ctx = Ctx { readCryptoCtx  :: CryptoCtx
               , writeCryptoCtx :: CryptoCtx
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
  let snf = fmtNum msgCounterLen $ unSeqNum seqNum
      aead' = C.aeadAppendHeader aead snf
      (ed, aead'') = C.aeadEncrypt aead' d
      at = C.aeadFinalize aead'' authTagLen
      msg = SecMessage { seqNumFmt = SeqNumFmt snf
                       , authTag = at
                       , ptyData = EncPtyData ed
                       }
      ctx' = CryptoCtx { seqNum = nextSeqNum seqNum, aead = aead'' }
  in (msg, ctx')


serialize :: SecMessage -> NetworkBytes
serialize SecMessage {seqNumFmt, authTag = AuthTag at, ptyData} =
  NetworkBytes $
     BS.singleton stx
  <> unSeqNumFmt seqNumFmt
  <> BS64.encode (at <> unEncPtyData ptyData)
  <> BS.singleton etx

-------------------------------------------------------------------------------
--                           network to PtyData
-------------------------------------------------------------------------------
deserialize :: ParseC SecMessage -> ParseC SecMessage -> NetworkBytes
             -> ([Either ErrText SecMessage], ParseC SecMessage)
deserialize p0 pc (NetworkBytes bs) = parse p0 pc bs

parse :: ParseC a -> ParseC a -> ByteString -> ([Either ErrText a], ParseC a)
parse p0 pc input = case pc input of
  A.Done "" r     -> ([r], p0)
  A.Done i r      -> first (r:) $ parse p0 p0 i
  A.Partial f     -> ([], f)
  A.Fail "" _ msg -> ([Left $ toS msg], p0)
  A.Fail i _ _    -> parse p0 p0 $ BS.drop 1 i

mkParseC :: Int -> ParseC SecMessage
mkParseC sz = A.parse $ do
  _ <- A.word8 stx
  sn <- A.take msgCounterLen
  (at, bs) <- fmap (BS.splitAt authTagLen) $ A.scan (succ sz) f >>= rdBase64
  _ <- A.word8 etx
  if BS.length bs > sz
    then pure $ Left $ "Message size of " <> show sz <> " exceeded"
    else pure $ Right $ SecMessage { seqNumFmt = SeqNumFmt sn
                                   , authTag = AuthTag at
                                   , ptyData = EncPtyData bs
                                   }
  where
    f :: Int -> Word8 -> Maybe Int
    f 0 _ = Nothing
    f n w | w /= etx = Just $ pred n
          | otherwise = Nothing

    rdBase64 :: ByteString -> A.Parser ByteString
    rdBase64 ys = case BS64.decode ys of
      Right v -> pure $! v
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

mkCryptoCtx :: Iv -> Secret -> Either ErrText CryptoCtx
mkCryptoCtx iv sec = CryptoCtx <$> pure (SeqNum 5678) <*> mkAEAD iv sec

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

-------------------------------------------------------------------------------
--                             Pipes interface
-------------------------------------------------------------------------------
toTransport :: CryptoCtx -> Pipe PtyData NetworkBytes IO ()
toTransport cctx = toT1 cctx >-> toT2

toT1 :: CryptoCtx -> Pipe PtyData SecMessage IO ()
toT1 ctx = do
  x <- await
  let (r, ctx') = mkMessage x ctx
  yield r
  toT1 ctx'

toT2 :: Pipe SecMessage NetworkBytes IO ()
toT2 = forever $ await >>= yield . serialize


fromTransport :: CryptoCtx -> Pipe NetworkBytes PtyData IO ()
fromTransport cctx = fromT1 >-> fromT2 cctx

fromT1 :: Pipe NetworkBytes SecMessage IO ()
fromT1 = from' pcc pcc
  where
    pcc = mkParseC maxMsgLen
    from' p0 p1 = do
      x <- await
      let (rs'es, p1') = deserialize p0 p1 x
      mapM_ (\v -> putText $ "WARN: " <> v) $ lefts rs'es
      mapM_ yield $ rights rs'es
      from' p0 p1'

fromT2 :: CryptoCtx -> Pipe SecMessage PtyData IO ()
fromT2 ctx = do
  x <- await
  case readMessage x ctx of
    Right (d, ctx') -> do yield d
                          fromT2 ctx'
    Left e -> do putText $ "Fatal error: " <> e

