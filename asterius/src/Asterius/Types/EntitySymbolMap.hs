{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Asterius.Types.EntitySymbolMap
  ( EntitySymbolMap
  , emptyESM
  , unitESM
  , elemESM
  , elemsESM
  , lookupESM
  , fromListESM
  , toListESM
  , (!)
  , filterWithKeyESM
  , filterESM
  , mapWithKeyESM
  , insertESM
  , restrictKeysESM
  , keysSetESM
  , keysESM
  , mapAccumESM
  , foldrWithKeyESM
  , mapKeysESM
  , toMapESM
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

-- TODOs
--------
-- * Fill in missing type class implementations
-- * Take @TerrorJack's advice and use a strict tuple in the map.
-- * Ensure that all operations behave as expected.
-- * Run ormolu on everything

-- | Map from 'EntitySymbol's to other things.
newtype EntitySymbolMap elt = EntitySymbolMap (UniqFM (EntitySymbol, elt))
  deriving newtype (Eq)
  deriving stock (Data)

instance (Show elt) => Show (EntitySymbolMap elt) where
  show = error "GEORGE: TODO"

instance Semigroup (EntitySymbolMap elt) where
  EntitySymbolMap m1 <> EntitySymbolMap m2 = EntitySymbolMap $ m1 <> m2

instance Monoid (EntitySymbolMap elt) where
  mempty = emptyESM

instance Binary elt => Binary (EntitySymbolMap elt) where
  get = error "GEORGE: TODO"
  put = error "GEORGE: TODO"
  put_ = error "GEORGE: TODO"

-- instance Generic (EntitySymbolMap elt) where
--   -- TODO
--   from = error "GEORGE: TODO"
--   to = error "GEORGE: TODO"
--
-- instance Binary elt => Binary (EntitySymbolMap elt) where
--   put = error "GEORGE: TODO"
--   get = error "GEORGE: TODO"

-- GEORGE: EntitySymbol is ofc an instance of Uniquable

{-# INLINE unitESM #-}
unitESM :: EntitySymbol -> elt -> EntitySymbolMap elt
unitESM k e = EntitySymbolMap $ unitUFM k (k,e)

{-# INLINE emptyESM #-}
emptyESM :: EntitySymbolMap elt
emptyESM = EntitySymbolMap emptyUFM

{-# INLINE elemESM #-}
elemESM :: EntitySymbol -> EntitySymbolMap elt -> Bool
elemESM k (EntitySymbolMap m) = elemUFM k m

-- | TODO: Improve implementation.
elemsESM :: EntitySymbolMap elt -> [elt]
elemsESM (EntitySymbolMap m) = map snd $ eltsUFM m

{-# INLINE lookupESM #-}
lookupESM :: EntitySymbol -> EntitySymbolMap elt -> Maybe elt
lookupESM k (EntitySymbolMap m) = snd <$> lookupUFM m k

-- | TODO: reduce usage.
{-# INLINE fromListESM #-}
fromListESM :: [(EntitySymbol, elt)] -> EntitySymbolMap elt
fromListESM = EntitySymbolMap
            . listToUFM
            . map (\(k,e) -> (k,(k,e)))

-- | TODO: reduce usage.
{-# INLINE toListESM #-}
toListESM :: EntitySymbolMap elt -> [(EntitySymbol, elt)]
toListESM (EntitySymbolMap m) = eltsUFM m

infixl 9 !
(!) :: HasCallStack => EntitySymbolMap elt -> EntitySymbol -> elt
(!) m k = case lookupESM k m of
  Just e -> e
  Nothing -> error "EntitySymbolMap.!: given key is not an element in the map"

-- | TODO: reduce usage.
viaList ::
  ([(EntitySymbol, elt1)] -> [(EntitySymbol, elt2)]) ->
  (EntitySymbolMap elt1 -> EntitySymbolMap elt2)
viaList f = fromListESM . f . toListESM

filterWithKeyESM ::
  (EntitySymbol -> elt -> Bool) ->
  EntitySymbolMap elt ->
  EntitySymbolMap elt
filterWithKeyESM f (EntitySymbolMap m) =
  EntitySymbolMap $ filterUFM (uncurry f) m

-- | TODO: reduce usage.
restrictKeysESM ::
  EntitySymbolMap elt -> Set.Set EntitySymbol -> EntitySymbolMap elt
restrictKeysESM m s = filterWithKeyESM (\k _ -> k `Set.member` s) m

mapWithKeyESM ::
  (EntitySymbol -> a -> b) ->
  EntitySymbolMap a ->
  EntitySymbolMap b
mapWithKeyESM fn = viaList (map (\(k,e) -> (k, fn k e)))

insertESM :: EntitySymbol -> elt -> EntitySymbolMap elt -> EntitySymbolMap elt
insertESM k e (EntitySymbolMap m) = EntitySymbolMap $ addToUFM m k (k,e)

-- | TODO: Notice that this one uses the 'Ord' instance for 'EntitySymbol'
-- (because it calls 'Set.fromList'). TODO: Reduce usage.
keysSetESM :: EntitySymbolMap elt -> Set.Set EntitySymbol
keysSetESM (EntitySymbolMap m) = Set.fromList $ map fst $ eltsUFM m

-- | TODO: Notice that this one uses the 'Ord' instance for 'EntitySymbol'
-- (because it calls 'sort', to ensure that the resulting keys are in ascending
-- order, to match the semantics of the corresponding function for maps). TODO:
-- Reduce usage.
keysESM :: EntitySymbolMap elt -> [EntitySymbol]
keysESM (EntitySymbolMap m) = sort $ map fst $ eltsUFM m

-- | TODO: Reduce usage.
mapAccumESM :: (a -> b -> (a, c)) -> a -> EntitySymbolMap b -> (a, EntitySymbolMap c)
mapAccumESM f a m
  | (keys, elts) <- unzip $ toListESM m
  , (acc, list)  <- mapAccumL f a elts
  = (acc, fromListESM $ keys `zip` list)

-- | TODO: Reduce usage. GEORGE: What about a strict variant?
foldrWithKeyESM :: (EntitySymbol -> a -> b -> b) -> b -> EntitySymbolMap a -> b
foldrWithKeyESM fn z = foldr (\(k,a) b -> fn k a b) z . toListESM

instance IsList (EntitySymbolMap elt) where
  type Item (EntitySymbolMap elt) = (EntitySymbol, elt)
  fromList = fromListESM
  toList = toListESM

-- | TODO: Reduce usage.
{-# INLINE filterESM #-}
filterESM :: (elt -> Bool) -> EntitySymbolMap elt -> EntitySymbolMap elt
filterESM p (EntitySymbolMap m) = EntitySymbolMap $ filterUFM (p . snd) m

-- | TODO: This looks like a very dangerous function to me. I'd prefer if we
-- didn't have to use it in the first place (whether it is custom or if it
-- comes from @Data.Map@).
mapKeysESM ::
  (EntitySymbol -> EntitySymbol) -> EntitySymbolMap elt -> EntitySymbolMap elt
mapKeysESM fn = viaList (map (\(k,e) -> (fn k, e)))

-- | TODO: Reduce usage.
{-# INLINE toMapESM #-}
toMapESM :: EntitySymbolMap elt -> Map.Map EntitySymbol elt
toMapESM = Map.fromList . toListESM

