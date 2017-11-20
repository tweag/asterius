{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}

module Asterius.Utils
  ( collect
  ) where

import Data.Data (Data, gmapQl)
import Data.Set
import GHC.Exts
import Type.Reflection

collect :: (Data t, Eq k, Ord k, Typeable k) => Proxy# k -> t -> Set k
collect pk t = case eqTypeRep (typeOf t) (f pk) of
  Just HRefl -> singleton t
  _          -> gmapQl union empty (collect pk) t
 where
  f :: Typeable t => Proxy# t -> TypeRep t
  {-# INLINE f #-}
  f _ = typeRep
