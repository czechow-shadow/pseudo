{-# LANGUAGE TemplateHaskell #-}

module MessageProp where

import Protolude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Message


prop_parser :: Property
prop_parser = forAll genOk $ \xs ->
  let xs' = map wrapStxEtx xs
      ys'' = map (Right . NetworkMessage . unNetworkBytes) xs'
      (ys, pctx') = first concat $ runState (mapM tokenizeM xs') emptyParseCtx
  in      label "Ctx"  (pctx' === emptyParseCtx)
     .&&. label "Cont" (ys === ys'')


prop_parser' :: Property
prop_parser' = forAll genPartOk $ \xs ->
  let ys'' = map NetworkMessage $
             filter (BS.singleton stx `BS.isPrefixOf`) $
             map unNetworkBytes xs
      (ys, pctx') = first concat $ runState (mapM tokenizeM xs) emptyParseCtx
  in      label "Ctx"  (pctx' === emptyParseCtx)
     .&&. label "Cont" (rights ys === ys'')

prop_parser'' :: Property
prop_parser'' = forAll genOk $ \xs ->
  let xs' = NetworkBytes (BS.singleton stx) : xs
        ++ [NetworkBytes $ BS.singleton etx]
      ys'' = BS.concat $ map unNetworkBytes xs'
      (ys, pctx') = first concat $ runState (mapM tokenizeM xs') emptyParseCtx
  in      label "Ctx"  (pctx' === emptyParseCtx)
     .&&. label "Cont" (ys === [Right $ NetworkMessage ys''])


wrapStxEtx :: NetworkBytes -> NetworkBytes
wrapStxEtx (NetworkBytes xs) =
  NetworkBytes $ BS.singleton stx <> xs <> BS.singleton etx


genOk :: Gen [NetworkBytes]
genOk = listOf genInner

genInner :: Gen NetworkBytes
genInner = NetworkBytes . BSC.pack <$> listOf g
  where
    g = elements $ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['A' .. 'Z']

genPartOk :: Gen [NetworkBytes]
genPartOk = do
  xs <- listOf genInner
  l <- choose (0, length xs)
  let (ys, zs) = first (map wrapStxEtx) $ splitAt l xs
  shuffle $ ys ++ zs
  

tests :: TestTree
tests = $(testGroupGenerator)

