{-# LANGUAGE BangPatterns #-}

module Asterius.Internals.Binary
  ( decodeMaybe,
    lazyMapPut,
    lazyMapGet,
  )
where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as LMap

{-# INLINE decode' #-}
decode' :: Binary a => BS.ByteString -> a
decode' buf = case runGetIncremental get of
  Partial k -> case k (Just buf) of
    Done _ _ r -> r
    _ -> error "Asterius.Internals.Binary.decode': failed to deserialize"
  _ ->
    error "Asterius.Internals.Binary.decode': failed to start deserialization"

{-# INLINE decodeMaybe #-}
decodeMaybe :: Binary a => BS.ByteString -> Maybe a
decodeMaybe buf = case runGetIncremental get of
  Partial k -> case k (Just buf) of
    Done _ _ !r -> Just r
    _ -> Nothing
  _ -> Nothing

{-# INLINE lazyMapPut #-}
lazyMapPut :: (Binary k, Binary v) => Map k v -> Put
lazyMapPut m =
  put (LMap.size m)
    *> LMap.foldrWithKey' (\k v acc -> put k *> put (encode v) *> acc) mempty m

{-# INLINE lazyMapGet #-}
lazyMapGet :: (Binary k, Binary v) => Get (Map k v)
lazyMapGet = do
  n <- get
  LMap.fromDistinctAscList <$> replicateM n ((,) <$> get <*> (decode' <$> get))
