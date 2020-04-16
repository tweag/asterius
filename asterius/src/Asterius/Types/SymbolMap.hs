{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    foldrWithKey,
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

import Asterius.Binary.Orphans ()
import Asterius.Types.EntitySymbol
import Binary
import Data.Data
import qualified Data.IntMap as IM
import Data.List (mapAccumL)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Exts (IsList (..))
import GHC.Stack
import Unique
import Prelude hiding (filter, lookup)

-- ----------------------------------------------------------------------------

-- TODOs

-- * Take @TerrorJack's advice and use a strict tuple in the map.

-- * Ensure that all operations behave as expected.

-- * Run ormolu on everything

-- | A map from 'EntitySymbol's to values @a@.
newtype SymbolMap a = SymbolMap (IM.IntMap (EntitySymbol, a))
  deriving newtype (Eq)
  deriving stock (Data)

instance (Show a) => Show (SymbolMap a) where
  showsPrec d m =
    showParen (d > 10) $
      showString "fromList " . shows (toList m)

instance Semigroup (SymbolMap a) where
  SymbolMap m1 <> SymbolMap m2 = SymbolMap $ m1 <> m2

instance Monoid (SymbolMap a) where
  mempty = empty
  mappend = (<>)

instance Binary a => Binary (SymbolMap a) where
  put_ bh m = put_ bh (toMap m) -- TODO: FIXME
  get bh = fromMap <$> get bh -- TODO: FIXME

-- Current HEAD (4187adb8d09933ac119269d5c3b020d96c8551fc)
-- does everything in ascending order. Is that of importance?

-- instance (GHC.Binary k, GHC.Binary v) => GHC.Binary (M.Map k v) where
--   put_ bh m =
--     GHC.put_ bh (M.size m)
--       *> for_ (M.toAscList m) (\(k, v) -> GHC.put_ bh k *> GHC.lazyPut bh v)
--   get bh =
--     fmap M.fromDistinctAscList $
--       GHC.get bh
--         >>= flip
--           replicateM
--           ((,) <$> GHC.get bh <*> GHC.lazyGet bh)

-- GEORGE: EntitySymbol is ofc an instance of Uniquable

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
singleton k e = SymbolMap $ IM.singleton (getKey $ getUnique k) (k, e)

-- | /O(n)/. Number of elements in the map.
{-# INLINE size #-}
size :: SymbolMap a -> Int
size (SymbolMap m) = IM.size m

-- | /O(min(n,W))/. Is the 'EntitySymbol' a member of the map?
{-# INLINE member #-}
member :: EntitySymbol -> SymbolMap a -> Bool
member k (SymbolMap m) = getKey (getUnique k) `IM.member` m

-- | /O(n)/. Return all elements of the map in the ascending order of the key
-- of the unique of their 'EntitySymbol'-key.
elems :: SymbolMap a -> [a]
elems (SymbolMap m) = map snd $ IM.elems m

-- | /O(min(n,W))/. Lookup the value at an 'EntitySymbol' in the map.
lookup :: EntitySymbol -> SymbolMap a -> Maybe a
lookup k (SymbolMap m) = snd <$> IM.lookup (getKey $ getUnique k) m

-- | /O(min(n,W))/. Find the value at an 'EntitySymbol'. Calls 'error' when the
-- element can not be found.
(!) :: HasCallStack => SymbolMap a -> EntitySymbol -> a
(!) m k = case lookup k m of
  Just e -> e
  Nothing -> error "SymbolMap.!: given key is not an element in the map"

infixl 9 !

-- | /O(n)/. Filter all keys/values that satisfy some predicate. NOTE: since we
-- use 'Key' for indexing and not 'EntitySymbol', the filtering happens on the
-- elements alone (which contain the corresponding 'EntitySymbol').
filterWithKey :: (EntitySymbol -> a -> Bool) -> SymbolMap a -> SymbolMap a
filterWithKey p (SymbolMap m) = SymbolMap $ IM.filter (uncurry p) m

-- | The restriction of a map to the keys in a set.
restrictKeys :: SymbolMap a -> Set.Set EntitySymbol -> SymbolMap a -- TODO: calculate perf (not /O(n+m)/)
restrictKeys m s = filterWithKey (\k _ -> k `Set.member` s) m -- TODO: improve implementation?

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (EntitySymbol -> a -> b) -> SymbolMap a -> SymbolMap b
mapWithKey fn = viaList (map (\(k, e) -> (k, fn k e))) -- TODO: how to avoid using viaList?

-- | /O(min(n,W))/. Insert a new key/value pair in the map. If the key is
-- already present in the map, the associated value is replaced with the
-- supplied value.
insert :: EntitySymbol -> a -> SymbolMap a -> SymbolMap a
insert k e (SymbolMap m) =
  SymbolMap $ IM.insert (getKey $ getUnique k) (k, e) m

-- | /O(n)/. The set of all keys of the map. NOTE: This function utilizes the
-- 'Ord' instance for 'EntitySymbol' (because it calls 'Set.fromList'
-- internally).
keysSet :: SymbolMap a -> Set.Set EntitySymbol
keysSet (SymbolMap m) = Set.fromList $ map fst $ IM.elems m

-- | /O(n)/. Return all 'EntitySymbol' keys of the map, in ascending order of
-- the key of their unique.
keys :: SymbolMap a -> [EntitySymbol]
keys (SymbolMap m) = map fst $ IM.elems m

-- | /O(n)/. Thread an accumulating argument through the map, in ascending
-- order of keys of uniques on the 'EntitySymbol' keys.
mapAccum :: (a -> b -> (a, c)) -> a -> SymbolMap b -> (a, SymbolMap c)
mapAccum f a m
  | (ks, elts) <- unzip $ toList m,
    (acc, list) <- mapAccumL f a elts = -- TODO: reduce usage?
    (acc, fromList $ ks `zip` list) -- TODO: Use the underlying mapAccum directly somehow?

-- | /O(n)/. Fold the keys and values in the map using the given
-- right-associative binary operator.
foldrWithKey :: (EntitySymbol -> a -> b -> b) -> b -> SymbolMap a -> b -- TODO: what about a strict variant?
foldrWithKey fn z = foldr (\(k, a) b -> fn k a b) z . toList -- TODO: how to avoid using toList?

-- | /O(n)/. Filter all values that satisfy a predicate.
{-# INLINE filter #-}
filter :: (a -> Bool) -> SymbolMap a -> SymbolMap a
filter p (SymbolMap m) = SymbolMap $ IM.filter (p . snd) m

-- | /O(n*log n)/. Apply a function to each key in a map. The size of the
-- result may be smaller if two old 'EntitySymbol's are mapped to the same new
-- 'EntitySymbol'. In this case the value at the greates of the original keys
-- is retained.
mapKeys :: (EntitySymbol -> EntitySymbol) -> SymbolMap a -> SymbolMap a -- TODO: do we really need this function?
mapKeys fn = viaList (map (\(k, e) -> (fn k, e))) -- TODO: how to avoid using viaList?

-- ----------------------------------------------------------------------------

-- | /O(n*log n)/. Build a symbol map from a list of key/value pairs.
{-# INLINE fromListSM #-}
fromListSM :: [(EntitySymbol, a)] -> SymbolMap a
fromListSM =
  SymbolMap
    . IM.fromList
    . map (\(k, e) -> (getKey $ getUnique k, (k, e)))

-- | /O(n)/. Convert a symbol map to a list of key/value pairs.
{-# INLINE toListSM #-}
toListSM :: SymbolMap a -> [(EntitySymbol, a)]
toListSM (SymbolMap m) = IM.elems m

-- | /O(n*log n)/. Convert a symbol map to a 'Map.Map'.
{-# INLINE toMap #-}
toMap :: SymbolMap a -> Map.Map EntitySymbol a
toMap = Map.fromList . toList

-- | /O(n*log n)/. Build a symbol map from a 'Map.Map'.
{-# INLINE fromMap #-}
fromMap :: Map.Map EntitySymbol a -> SymbolMap a
fromMap = fromList . Map.toList

-- ----------------------------------------------------------------------------

-- | TODO: reduce usage.
viaList ::
  ([(EntitySymbol, a)] -> [(EntitySymbol, b)]) ->
  (SymbolMap a -> SymbolMap b)
viaList f = fromList . f . toList
