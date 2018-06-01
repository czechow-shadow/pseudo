module Protolude.Extended where

import           Protolude

import           Data.List       (unzip)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO    as T


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left e)  = Left $ f e
mapLeft _ (Right x) = Right x

qtn :: Text -> Text
qtn xs = "'" <> xs <> "'"

duplicates :: (Ord a) => [a] -> [a]
duplicates xs = sortBy cf $ M.keys $ M.filter (>1) $ foldl f M.empty xs
  where
    f m k = M.insertWith (+) k (1 :: Int) m
    cf x y = compare (M.lookup x order) (M.lookup y order)
    order = M.fromList $ zip (ordNub xs) [1 :: Int ..]

hPutText :: MonadIO m => Handle -> Text -> m ()
hPutText = \h -> liftIO . T.hPutStrLn h

unzip :: [(a, b)] -> ([a], [b])
unzip = Data.List.unzip

