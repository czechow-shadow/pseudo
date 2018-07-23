module Protolude.Extended where

import           Protolude

import           Data.List       (unzip)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO    as T


type ErrText = Text


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

liftEither :: MonadError e m => Either e a -> m a
liftEither (Right x) = pure x
liftEither (Left e)  = throwError e


onExceptionE :: ExceptT e IO a -> ExceptT e IO b -> ExceptT e IO a
onExceptionE action handler =
  ExceptT $ (runExceptT action) `onException` runExceptT handler


bracketE :: ExceptT e IO a
         -> (a -> ExceptT e IO c)
         -> (a -> ExceptT e IO b)
         -> ExceptT e IO b
bracketE acquire release action =
  ExceptT $ mask $ \restore -> do -- IO monad
    runExceptT acquire >>= \case
      Right r -> do
        res <- restore (runExceptT $ action r) `onException` (runExceptT $ release r)
        void $ runExceptT $ release r
        pure res
      Left e -> pure $ Left e

bracketOnErrorE :: ExceptT e IO a
                -> (a -> ExceptT e IO c)
                -> (a -> ExceptT e IO b)
                -> ExceptT e IO b
bracketOnErrorE acquire release action =
  ExceptT $ mask $ \restore -> do -- IO monad
    runExceptT acquire >>= \case
      Right r -> do
        res <- restore (runExceptT $ action r) `onException` (runExceptT $ release r)
        case res of
          Right _ -> pure res
          Left _ -> do void $ runExceptT $ release r
                       pure res
      Left e -> pure $ Left e

handleIOEx :: IO a -> ExceptT ErrText IO a
handleIOEx action = liftIO (try action) >>= \case
  Right v -> pure v
  Left (e :: IOException) -> throwError $ show $ e
