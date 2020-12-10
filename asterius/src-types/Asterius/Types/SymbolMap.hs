{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Asterius.Types.SymbolMap
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- The @'SymbolMap' a@ type represents a finite map/dictionary from keys of
-- type 'EntitySymbol' to values of type @a@. Internally it is represented as
-- an 'IM.IntMap' of , where the key of the unique of the 'EntitySymbol' is used
-- for the indexing. In order to be able to access the 'EntitySymbol'
-- corresponding to each entry, we also store it alongside each element.
module Asterius.Types.SymbolMap
  ( -- * SymbolMap type
    SymbolMap,

    -- * Construction
    empty,
    singleton,

    -- * Query
    member,
    lookup,
    (!),

    -- * Size
    size,

    -- * Insertion
    insert,

    -- * Filtering
    filterWithKey,
    filter,
    restrictKeys,

    -- * Folds and Maps
    foldrWithKey',
    mapWithKey,
    mapAccum,
    mapKeys,

    -- * Conversion
    elems,
    keys,
    keysSet,

    -- ** Lists
    toList,
    fromList,

    -- ** Maps
    toMap,
    fromMap,
  )
where

import Asterius.Types.EntitySymbol
import qualified Asterius.Types.SymbolSet as SS
import Binary
import Control.DeepSeq
import Control.Monad
import Data.Data
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as Map
import GHC.Exts (IsList (..))
import GHC.Stack
import Prelude hiding (filter, lookup)

-- | A map from 'EntitySymbol's to values @a@.
newtype SymbolMap a = SymbolMap (IM.IntMap (EntitySymbol, a))
  deriving newtype (Eq, Monoid, NFData)
  deriving stock (Data)

instance Semigroup (SymbolMap a) where
  SymbolMap m0 <> SymbolMap m1 =
    SymbolMap $
      IM.unionWithKey
        (\sym _ _ -> error $ "Duplicate symbol: " <> show sym)
        m0
        m1

instance Show a => Show (SymbolMap a) where
  showsPrec d m =
    showParen (d > 10) $
      showString "fromList " . shows (toListSM m)

instance Binary a => Binary (SymbolMap a) where
  put_ bh m =
    put_ bh (size m)
      *> forM_ (toListSM m) (\(k, v) -> put_ bh k *> lazyPut bh v)
  get bh = fromListSM <$> do
    n <- get bh
    replicateM n $ (,) <$> get bh <*> lazyGet bh

instance IsList (SymbolMap a) where
  type Item (SymbolMap a) = (EntitySymbol, a)
  fromList = fromListSM
  toList = toListSM

-- ----------------------------------------------------------------------------

-- | /O(1)/. The empty map.
{-# INLINE empty #-}
empty :: SymbolMap a
empty = SymbolMap IM.empty

-- | /O(1)/. A map containing a single association.
{-# INLINE singleton #-}
singleton :: EntitySymbol -> a -> SymbolMap a
singleton k e = SymbolMap $ IM.singleton (getKeyES k) (k, e)

-- | /O(n)/. Number of elements in the map.
{-# INLINE size #-}
size :: SymbolMap a -> Int
size (SymbolMap m) = IM.size m

-- | /O(min(n,W))/. Is the 'EntitySymbol' a member of the map?
{-# INLINE member #-}
member :: EntitySymbol -> SymbolMap a -> Bool
member k (SymbolMap m) = getKeyES k `IM.member` m

-- | /O(n)/. Return all elements of the map in the ascending order of the key
-- of the unique of their 'EntitySymbol'-key.
elems :: SymbolMap a -> [a]
elems (SymbolMap m) = map snd $ IM.elems m

-- | /O(min(n,W))/. Lookup the value at an 'EntitySymbol' in the map.
lookup :: EntitySymbol -> SymbolMap a -> Maybe a
lookup k (SymbolMap m) = snd <$> IM.lookup (getKeyES k) m

-- | /O(min(n,W))/. Find the value at an 'EntitySymbol'. Calls 'error' when the
-- element can not be found.
(!) :: HasCallStack => SymbolMap a -> EntitySymbol -> a
(!) m k = case lookup k m of
  Just e -> e
  Nothing ->
    error $
      "SymbolMap.!: given key ("
        ++ show k
        ++ ") is not an element in the map"

infixl 9 !

-- | /O(n)/. Filter all keys/values that satisfy some predicate. NOTE: since we
-- use 'Key' for indexing and not 'EntitySymbol', the filtering happens on the
-- elements alone (which contain the corresponding 'EntitySymbol').
filterWithKey :: (EntitySymbol -> a -> Bool) -> SymbolMap a -> SymbolMap a
filterWithKey p (SymbolMap m) = SymbolMap $ IM.filter (uncurry p) m

-- | The restriction of a map to the keys in a set.
restrictKeys :: SymbolMap a -> SS.SymbolSet -> SymbolMap a
restrictKeys (SymbolMap m) s = SymbolMap $ IM.restrictKeys m (SS.toIntSet s)

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (EntitySymbol -> a -> b) -> SymbolMap a -> SymbolMap b
mapWithKey fn (SymbolMap m) =
  SymbolMap $ IM.mapWithKey (\_ (k, e) -> (k, fn k e)) m

-- | /O(min(n,W))/. Insert a new key/value pair in the map. If the key is
-- already present in the map, the associated value is replaced with the
-- supplied value.
insert :: EntitySymbol -> a -> SymbolMap a -> SymbolMap a
insert k e (SymbolMap m) = SymbolMap $ IM.insert (getKeyES k) (k, e) m

-- | /O(n*log n)/. The set of all keys of the map.
{-# INLINE keysSet #-}
keysSet :: SymbolMap a -> SS.SymbolSet
keysSet = SS.fromList . keys

-- | /O(n)/. Return all 'EntitySymbol' keys of the map, in ascending order of
-- the key of their unique.
keys :: SymbolMap a -> [EntitySymbol]
keys (SymbolMap m) = map fst $ IM.elems m

-- | /O(n)/. Thread an accumulating argument through the map, in ascending
-- order of keys of uniques on the 'EntitySymbol' keys.
mapAccum :: (a -> b -> (a, c)) -> a -> SymbolMap b -> (a, SymbolMap c)
mapAccum f z (SymbolMap m) =
  SymbolMap <$> IM.mapAccum (\a (k, e) -> fmap (k,) $ f a e) z m

-- | /O(n)/. Fold the keys and values in the map using the given
-- right-associative binary operator. This is a strict variant: each
-- application of the operator is evaluated before using the result in the next
-- application. This function is strict in the starting value.
foldrWithKey' :: (EntitySymbol -> a -> b -> b) -> b -> SymbolMap a -> b
foldrWithKey' f z (SymbolMap m) = IM.foldrWithKey' (\_ (k, e) b -> f k e b) z m

-- | /O(n)/. Filter all values that satisfy a predicate.
{-# INLINE filter #-}
filter :: (a -> Bool) -> SymbolMap a -> SymbolMap a
filter p (SymbolMap m) = SymbolMap $ IM.filter (p . snd) m

-- | /O(n*log n)/. Apply a function to each key in a map. The size of the
-- result may be smaller if two old 'EntitySymbol's are mapped to the same new
-- 'EntitySymbol'. In this case the value at the greates of the original keys
-- is retained.
mapKeys :: (EntitySymbol -> EntitySymbol) -> SymbolMap a -> SymbolMap a
mapKeys fn = fromListSM . (map (\(k, e) -> (fn k, e))) . toListSM

-- GEORGE: Given that EntitySymbol appears both in a co- and a contra- variant
-- position in the function, there is no direct way to utilize IM.mapKeys for
-- implementing mapKeys (getUnique is irreversible). TODO: reduce usage.

-- ----------------------------------------------------------------------------

instance Functor SymbolMap where
  fmap f (SymbolMap m) = SymbolMap $ IM.map (\(k, e) -> (k, f e)) m

instance Foldable SymbolMap where
  foldr f z (SymbolMap m) = IM.foldr (\(_, e) b -> f e b) z m

instance Traversable SymbolMap where
  traverse f (SymbolMap m) =
    SymbolMap <$> traverse (\(k, x) -> fmap (\e -> (k, e)) (f x)) m

-- | /O(n*log n)/. Build a symbol map from a list of key/value pairs.
{-# INLINE fromListSM #-}
fromListSM :: [(EntitySymbol, a)] -> SymbolMap a
fromListSM =
  SymbolMap
    . IM.fromList
    . map (\(k, e) -> (getKeyES k, (k, e)))

-- | /O(n)/. Convert a symbol map to a list of key/value pairs.
{-# INLINE toListSM #-}
toListSM :: SymbolMap a -> [(EntitySymbol, a)]
toListSM (SymbolMap m) = IM.elems m

-- | /O(n*log n)/. Convert a symbol map to a 'Map.Map'.
{-# INLINE toMap #-}
toMap :: SymbolMap a -> Map.Map EntitySymbol a
toMap = Map.fromList . toListSM

-- | /O(n*log n)/. Build a symbol map from a 'Map.Map'.
{-# INLINE fromMap #-}
fromMap :: Map.Map EntitySymbol a -> SymbolMap a
fromMap = fromListSM . Map.toList
