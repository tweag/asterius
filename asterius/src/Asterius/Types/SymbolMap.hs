{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}



-- See http://hackage.haskell.org/package/ghc-8.6.5/docs/Unique.html#t:Uniquable
--

-- GEORGE: The order of things can be very different iiuc:
--
-- Conversion from Map k v (e.g. elems, toList, ...) always seem to create
-- results in ascending key order. Even `elems` that only returns the elements
-- of the map, it always gives them in ascending order of their keys (even if
-- the keys are not part of the result).
--
-- The same seems to be the case with IntMap (which is used internally by
-- UniqFM). BUT, EntitySymbol is a synony of FastString and the Uniquable
-- instance for FastString does not necessarily agree with lexicographic order:
--
--   instance Uniquable FastString where
--     getUnique fs = mkUniqueGrimily (uniqueOfFS fs)
--
-- This means that our implementations here do not agree with the corresponding
-- ones in Data.Map. This could lead to problems.

module Asterius.Types.SymbolMap
  ( SymbolMap
  , empty
  , singleton
  , member
  , elems
  , lookup
  , fromList
  , toList
  , (!)
  , filterWithKey
  , filter
  , mapWithKey
  , insert
  , restrictKeys
  , keysSet
  , keys
  , mapAccum
  , foldrWithKey
  , mapKeys
  , toMap
  )
where

import UniqFM
import Data.Data
import Binary
import Asterius.Types.EntitySymbol
import GHC.Stack
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (mapAccumL, sort)
import GHC.Exts (IsList(..))
import Asterius.Binary.Orphans ()
import Prelude hiding (lookup, filter)

-- ----------------------------------------------------------------------------

-- TODOs
-- * Take @TerrorJack's advice and use a strict tuple in the map.
-- * Ensure that all operations behave as expected.
-- * Run ormolu on everything

-- | Map from 'EntitySymbol's to other things.
newtype SymbolMap elt = SymbolMap (UniqFM (EntitySymbol, elt))
  deriving newtype (Eq)
  deriving stock (Data)

instance (Show elt) => Show (SymbolMap elt) where
  showsPrec d m = showParen (d > 10) $
    showString "fromList " . shows (toList m)

instance Semigroup (SymbolMap elt) where
  SymbolMap m1 <> SymbolMap m2 = SymbolMap $ m1 <> m2

instance Monoid (SymbolMap elt) where
  mempty = empty

instance Binary elt => Binary (SymbolMap elt) where
  put_ bh m = put_ bh (toMap m)
  get bh = fromMap <$> get bh

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

instance IsList (SymbolMap elt) where
  type Item (SymbolMap elt) = (EntitySymbol, elt)
  fromList = fromListSM
  toList = toListSM

-- ----------------------------------------------------------------------------

{-# INLINE singleton #-}
singleton :: EntitySymbol -> elt -> SymbolMap elt
singleton k e = SymbolMap $ unitUFM k (k,e)

{-# INLINE empty #-}
empty :: SymbolMap elt
empty = SymbolMap emptyUFM

{-# INLINE member #-}
member :: EntitySymbol -> SymbolMap elt -> Bool
member k (SymbolMap m) = elemUFM k m

-- | TODO: Improve implementation.
elems :: SymbolMap elt -> [elt]
elems (SymbolMap m) = map snd $ eltsUFM m

{-# INLINE lookup #-}
lookup :: EntitySymbol -> SymbolMap elt -> Maybe elt
lookup k (SymbolMap m) = snd <$> lookupUFM m k

infixl 9 !
(!) :: HasCallStack => SymbolMap elt -> EntitySymbol -> elt
(!) m k = case lookup k m of
  Just e -> e
  Nothing -> error "SymbolMap.!: given key is not an element in the map"

filterWithKey :: (EntitySymbol -> a -> Bool) -> SymbolMap a -> SymbolMap a
filterWithKey f (SymbolMap m) = SymbolMap $ filterUFM (uncurry f) m

-- | TODO: reduce usage.
restrictKeys :: SymbolMap elt -> Set.Set EntitySymbol -> SymbolMap elt
restrictKeys m s = filterWithKey (\k _ -> k `Set.member` s) m

mapWithKey :: (EntitySymbol -> a -> b) -> SymbolMap a -> SymbolMap b
mapWithKey fn = viaList (map (\(k,e) -> (k, fn k e)))

insert :: EntitySymbol -> elt -> SymbolMap elt -> SymbolMap elt
insert k e (SymbolMap m) = SymbolMap $ addToUFM m k (k,e)

-- | TODO: Notice that this one uses the 'Ord' instance for 'EntitySymbol'
-- (because it calls 'Set.fromList'). TODO: Reduce usage.
keysSet :: SymbolMap elt -> Set.Set EntitySymbol
keysSet (SymbolMap m) = Set.fromList $ map fst $ eltsUFM m

-- | TODO: Notice that this one uses the 'Ord' instance for 'EntitySymbol'
-- (because it calls 'sort', to ensure that the resulting keys are in ascending
-- order, to match the semantics of the corresponding function for maps). TODO:
-- Reduce usage.
keys :: SymbolMap elt -> [EntitySymbol]
keys (SymbolMap m) = sort $ map fst $ eltsUFM m

-- | TODO: Reduce usage.
mapAccum :: (a -> b -> (a, c)) -> a -> SymbolMap b -> (a, SymbolMap c)
mapAccum f a m
  | (ks, elts) <- unzip $ toList m
  , (acc, list)  <- mapAccumL f a elts
  = (acc, fromList $ ks `zip` list)

-- | TODO: Reduce usage. GEORGE: What about a strict variant?
foldrWithKey :: (EntitySymbol -> a -> b -> b) -> b -> SymbolMap a -> b
foldrWithKey fn z = foldr (\(k,a) b -> fn k a b) z . toList

-- | TODO: Reduce usage.
{-# INLINE filter #-}
filter :: (elt -> Bool) -> SymbolMap elt -> SymbolMap elt
filter p (SymbolMap m) = SymbolMap $ filterUFM (p . snd) m

-- | TODO: This looks like a very dangerous function to me. I'd prefer if we
-- didn't have to use it in the first place (whether it is custom or if it
-- comes from @Data.Map@).
mapKeys :: (EntitySymbol -> EntitySymbol) -> SymbolMap elt -> SymbolMap elt
mapKeys fn = viaList (map (\(k,e) -> (fn k, e)))

-- ----------------------------------------------------------------------------

-- | TODO: reduce usage.
{-# INLINE fromListSM #-}
fromListSM :: [(EntitySymbol, elt)] -> SymbolMap elt
fromListSM = SymbolMap
            . listToUFM
            . map (\(k,e) -> (k,(k,e)))

-- | TODO: reduce usage.
{-# INLINE toListSM #-}
toListSM :: SymbolMap elt -> [(EntitySymbol, elt)]
toListSM (SymbolMap m) = eltsUFM m

-- | TODO: Reduce usage.
{-# INLINE toMap #-}
toMap :: SymbolMap elt -> Map.Map EntitySymbol elt
toMap = Map.fromList . toList

-- | TODO: Reduce usage.
{-# INLINE fromMap #-}
fromMap :: Map.Map EntitySymbol elt -> SymbolMap elt
fromMap = fromList . Map.toList

-- ----------------------------------------------------------------------------

-- | TODO: reduce usage.
viaList ::
  ([(EntitySymbol, elt1)] -> [(EntitySymbol, elt2)]) ->
  (SymbolMap elt1 -> SymbolMap elt2)
viaList f = fromList . f . toList

