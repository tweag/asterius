{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Asterius.Internals.DList
  ( DList
  , fromList
  , toList
  , singleton
  , cons
  , foldr
  , map
  ) where

import Control.Monad
import Data.Coerce
import Data.Monoid
import qualified GHC.Exts
import Prelude hiding (foldr, map)
import qualified Prelude

newtype DList a =
  DList (Endo [a])
  deriving (Semigroup, Monoid)

instance GHC.Exts.IsList (DList a) where
  type Item (DList a) = a
  {-# INLINE fromList #-}
  fromList = fromList
  {-# INLINE toList #-}
  toList = toList

instance Functor DList where
  {-# INLINE fmap #-}
  fmap = map

{-# INLINE fromList #-}
fromList :: [a] -> DList a
fromList = coerce . (<>)

{-# INLINE toList #-}
toList :: DList a -> [a]
toList = ($ []) . appEndo . coerce

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton = coerce . (:)

{-# INLINE cons #-}
cons :: a -> DList a -> DList a
cons = (<>) . singleton

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> DList a -> b
foldr f b = Prelude.foldr f b . toList

{-# INLINE map #-}
map :: (a -> b) -> DList a -> DList b
map f = foldr (cons . f) mempty
