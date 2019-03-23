module Asterius.Internals.Binary
  ( lazyMapPut
  , lazyMapGet
  ) where

import Data.Binary
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as LMap

{-# INLINE lazyMapPut #-}
lazyMapPut :: (Binary k, Binary v) => Map k v -> Put
lazyMapPut = put . LMap.map encode

{-# INLINE lazyMapGet #-}
lazyMapGet :: (Binary k, Binary v) => Get (Map k v)
lazyMapGet = LMap.map decode <$> get
