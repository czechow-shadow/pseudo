{-# LANGUAGE TemplateHaskell #-}

module MessageProp where

import           Protolude
import           Protolude.Extended

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Crypto.Cipher.Types
import qualified Data.ByteString           as BS

import           Test.QuickCheck.Instances ()

import           Message

data ParseCtx = ParseCtx { p0 :: ParseC SecMessage
                         , pc :: ParseC SecMessage
                         }
mkParseCtx :: ParseCtx
mkParseCtx = ParseCtx ppc ppc
  where
    ppc = mkParseC 8192

prop_roundtrip :: Property
prop_roundtrip = forAll genSecMessages $ \ms ->
  let nbs  = map serialize ms
      ms'e = concat $ evalState (mapM deserializeM nbs) mkParseCtx
  in label "Cont" (map Right ms === ms'e)

prop_roundtrip_distorted :: Property
prop_roundtrip_distorted = forAll genSecMessages $ \ms ->
  let nbs  = map NetworkBytes
           $ intersperse "GARBAGE"
           $ map (unNetworkBytes . serialize) ms
      ms'e = concat $ evalState (mapM deserializeM nbs) mkParseCtx
  in label "Cont" (map Right ms === ms'e)


prop_roundtrip_chopped :: Property
prop_roundtrip_chopped = forAll genSecMessages $ \ms ->
                forAll genChunks $ \chks ->
  let nbs = map NetworkBytes
          $ chunksOf (cycle chks)
          $ BS.concat $ map (unNetworkBytes . serialize) ms
      ms'e = concat $ evalState (mapM deserializeM nbs) mkParseCtx
  in label "Cont" (map Right ms === ms'e)


deserializeM :: (MonadState ParseCtx m) =>
             NetworkBytes -> m [Either ErrText SecMessage]
deserializeM xs = do
  ParseCtx{p0, pc} <- get
  let (res, pc') = deserialize p0 pc xs
  put $ ParseCtx p0 pc'
  return res

genChunks :: Gen [Int]
genChunks = listOf1 $ choose (1, 128)


genSeqNumFmt :: Gen SeqNumFmt
genSeqNumFmt = fmap (SeqNumFmt . toS) $ vectorOf 4 $ elements ['0' .. '9']

genAuthTag :: Gen AuthTag
genAuthTag = fmap (AuthTag . BS.pack) $ vectorOf 16 arbitrary

genEncPtyData :: Gen EncPtyData
genEncPtyData = EncPtyData <$> arbitrary

genSecMessage :: Gen SecMessage
genSecMessage = SecMessage <$> genSeqNumFmt <*> genAuthTag <*> genEncPtyData

genSecMessages :: Gen [SecMessage]
genSecMessages = listOf genSecMessage



tests :: TestTree
tests = $(testGroupGenerator)

chunksOf :: [Int] -> ByteString -> [ByteString]
chunksOf [] xs     = [xs]
chunksOf _ ""      = []
chunksOf (l:ls) xs = BS.take l xs : chunksOf ls (BS.drop l xs)


