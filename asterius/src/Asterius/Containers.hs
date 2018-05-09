{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Asterius.Containers
  ( HashMap(..)
  , hashMapInsert
  , hashMapLookup
  , (!)
  , hashMapFromListWith
  , hashMapToList
  , hashMapKeys
  , hashMapElems
  , hashMapSize
  , hashMapUnionWith
  , hashMapUnions
  , HashSet(..)
  , hashSetNull
  , hashSetDifference
  , hashSetUnions
  ) where

import qualified Data.ByteString.Short as SBS
import Data.Data (Data)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.List
import Data.Serialize
import qualified Data.Vector as V
import GHC.Exts
import GHC.Stack

newtype HashMap k v = HashMap
  { unHashMap :: HM.HashMap k v
  } deriving (Functor, Semigroup, Monoid, Data, IsList)

instance (Ord k, Show k, Show v) => Show (HashMap k v) where
  showsPrec p = showsPrec p . sortOn fst . hashMapToList
  {-# INLINE showsPrec #-}

instance (Eq k, Hashable k, Serialize k, Serialize v) =>
         Serialize (HashMap k v) where
  put = put . hashMapToList
  {-# INLINE put #-}
  get = fromList <$> get
  {-# INLINE get #-}

{-# INLINE hashMapInsert #-}
hashMapInsert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
hashMapInsert k v (HashMap m0) = coerce $ HM.insert k v m0

{-# INLINE hashMapLookup #-}
hashMapLookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
hashMapLookup k = HM.lookup k . coerce

{-# INLINE (!) #-}
(!) :: (HasCallStack, Eq k, Hashable k, Show k) => HashMap k v -> k -> v
(!) m k =
  case hashMapLookup k m of
    Just v -> v
    _ -> error $ "Key not found: " <> show k

{-# INLINE hashMapFromListWith #-}
hashMapFromListWith ::
     (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
hashMapFromListWith op l = coerce $ HM.fromListWith op l

{-# INLINE hashMapToList #-}
hashMapToList :: HashMap k v -> [(k, v)]
hashMapToList = HM.toList . unHashMap

{-# INLINE hashMapKeys #-}
hashMapKeys :: HashMap k v -> [k]
hashMapKeys = HM.keys . unHashMap

{-# INLINE hashMapElems #-}
hashMapElems :: HashMap k v -> [v]
hashMapElems = HM.elems . unHashMap

{-# INLINE hashMapSize #-}
hashMapSize :: HashMap k v -> Int
hashMapSize = HM.size . unHashMap

{-# INLINE hashMapUnionWith #-}
hashMapUnionWith ::
     (Eq k, Hashable k)
  => (v -> v -> v)
  -> HashMap k v
  -> HashMap k v
  -> HashMap k v
hashMapUnionWith op (HashMap m0) (HashMap m1) = coerce $ HM.unionWith op m0 m1

{-# INLINE hashMapUnions #-}
hashMapUnions :: (Eq k, Hashable k) => [HashMap k v] -> HashMap k v
hashMapUnions = HashMap . HM.unions . coerce

newtype HashSet k =
  HashSet (HS.HashSet k)
  deriving (Eq, Foldable, Semigroup, Monoid, IsList)

{-# INLINE hashSetNull #-}
hashSetNull :: HashSet k -> Bool
hashSetNull = HS.null . coerce

{-# INLINE hashSetDifference #-}
hashSetDifference :: (Eq k, Hashable k) => HashSet k -> HashSet k -> HashSet k
hashSetDifference s0 s1 = coerce $ HS.difference (coerce s0) (coerce s1)

{-# INLINE hashSetUnions #-}
hashSetUnions :: (Eq k, Hashable k) => [HashSet k] -> HashSet k
hashSetUnions = coerce . HS.unions . coerce

instance (Ord k, Hashable k, Show k) => Show (HashSet k) where
  showsPrec p = showsPrec p . sort . toList
  {-# INLINE showsPrec #-}

instance Serialize a => Serialize (V.Vector a) where
  put v = put (V.length v) *> V.mapM_ put v
  {-# INLINE put #-}
  get = do
    len <- get
    V.replicateM len get
  {-# INLINE get #-}

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt salt = hashWithSalt salt . toList
  {-# INLINE hashWithSalt #-}

instance Serialize SBS.ShortByteString where
  put sbs = put (SBS.length sbs) *> putShortByteString sbs
  {-# INLINE put #-}
  get = get >>= getShortByteString
  {-# INLINE get #-}
