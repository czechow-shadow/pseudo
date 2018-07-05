{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LibProp where

import           Protolude

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import qualified Data.Text             as T

import           Lib


instance Arbitrary Text where
   arbitrary = show <$> (arbitrary :: Gen Int)


prop_parser :: Property
prop_parser = forAll arbitrary $ \(xs :: [Text]) ->
   let xs' = map wrapStxEtx xs
       ys'' = map (Right . Message) xs'
       (ys, pctx') = first concat $ runState (mapM receiveM xs') emptyParseCtx
   in      label "Ctx" (pctx' === emptyParseCtx)
      .&&. label "Cont" (ys === ys'')


prop_parser' :: Property
prop_parser' = forAll genPartOk $ \(xs :: [Text]) ->
   let ys'' = map Message $ filter ("<" `T.isPrefixOf`) xs
       (ys, pctx') = first concat $ runState (mapM receiveM xs) emptyParseCtx
   in      label "Ctx" (pctx' === emptyParseCtx)
      .&&. label "Cont" (rights ys === ys'')

prop_parser'' :: Property
prop_parser'' = forAll (pure []) $ \(xs :: [Text]) ->
   let xs' = "<" : xs ++ [">"]
       ys'' = T.concat xs'
       (ys, pctx') = first concat $ runState (mapM receiveM xs') emptyParseCtx
   in      label "Ctx" (pctx' === emptyParseCtx)
      .&&. label "Cont" (ys === [Right $ Message ys''])


wrapStxEtx :: Text -> Text
wrapStxEtx xs = T.singleton '<' <> xs <> T.singleton '>'



genPartOk :: Gen [Text]
genPartOk = do
  xs <- arbitrary
  l <- choose (0, length xs)
  let (ys, zs) = first (map wrapStxEtx) $ splitAt l xs
  shuffle $ ys ++ zs


tests :: TestTree
tests = $(testGroupGenerator)

