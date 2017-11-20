{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Serialize.Cereal.Orphans where

import qualified Data.ByteString.Short as SBS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Serialize
import qualified Data.Vector as V
import Foreign.C

instance Serialize SBS.ShortByteString where
  {-# INLINE put #-}
  put = put . SBS.fromShort
  {-# INLINE get #-}
  get = SBS.toShort <$> get

instance Serialize a => Serialize (V.Vector a) where
  {-# INLINE put #-}
  put = put . V.toList
  {-# INLINE get #-}
  get = V.fromList <$> get

instance (Eq k, Hashable k, Serialize k, Serialize v) =>
         Serialize (HM.HashMap k v) where
  {-# INLINE put #-}
  put = put . HM.toList
  {-# INLINE get #-}
  get = HM.fromList <$> get

instance (Eq k, Hashable k, Serialize k) => Serialize (HS.HashSet k) where
  {-# INLINE put #-}
  put = put . HS.toList
  {-# INLINE get #-}
  get = HS.fromList <$> get

instance Serialize CFloat where
  {-# INLINE put #-}
  put = put . (\(CFloat x) -> x)
  {-# INLINE get #-}
  get = CFloat <$> get

instance Serialize CDouble where
  {-# INLINE put #-}
  put = put . (\(CDouble x) -> x)
  {-# INLINE get #-}
  get = CDouble <$> get
