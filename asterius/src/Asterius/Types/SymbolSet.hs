{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Asterius.Types.SymbolSet
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- The 'SymbolSet' type represents a set of elements of type 'EntitySymbol'.
-- Internally it is represented as an 'IM.IntMap', mapping the keys of the
-- uniques of the 'EntitySymbol's to the 'EntitySymbol's themselves.
module Asterius.Types.SymbolSet
  ( -- * SymbolSet type
    SymbolSet,

    -- * Construction
    empty,
    singleton,

    -- * Query
    null,

    -- * Combine
    union,
    unions,
    difference,

    -- * Folds and Maps
    foldr',

    -- * Conversion
    toIntSet,

    -- ** Lists
    fromList,
    toList,
  )
where

import Asterius.Types.EntitySymbol
import Data.Data
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as IS
import GHC.Exts (IsList (..))
import Prelude hiding (null)

-- | A set of 'EntitySymbol's.
newtype SymbolSet = SymbolSet {fromSymbolSet :: IM.IntMap EntitySymbol}
  deriving newtype (Eq, Semigroup, Monoid)
  deriving stock (Data)

instance Show SymbolSet where
  showsPrec p (SymbolSet s) =
    showParen (p > 10) $
      showString "fromList " . shows (toList s)

instance IsList SymbolSet where
  type Item SymbolSet = EntitySymbol
  fromList = fromListSS
  toList = toListSS

-- ----------------------------------------------------------------------------

-- | /O(1)/. The empty set.
{-# INLINE empty #-}
empty :: SymbolSet
empty = SymbolSet IM.empty

-- | /O(1)/. A set of one element.
{-# INLINE singleton #-}
singleton :: EntitySymbol -> SymbolSet
singleton e = SymbolSet $ IM.singleton (getKeyES e) e

-- | /O(n+m)/. The union of two sets.
{-# INLINE union #-}
union :: SymbolSet -> SymbolSet -> SymbolSet
union (SymbolSet s1) (SymbolSet s2) = SymbolSet (IM.union s1 s2)

-- | The union of a list of sets.
unions :: Foldable.Foldable f => f SymbolSet -> SymbolSet
unions xs = Foldable.foldl' union empty xs

-- | /O(1)/. Is the set empty?
{-# INLINE null #-}
null :: SymbolSet -> Bool
null = IM.null . fromSymbolSet

-- | /O(n+m)/. Difference between two sets.
{-# INLINE difference #-}
difference :: SymbolSet -> SymbolSet -> SymbolSet
difference (SymbolSet s1) (SymbolSet s2) = SymbolSet (s1 `IM.difference` s2)

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator. This is a strict variant: each application of the operator
-- is evaluated before using the result in the next application. This function
-- is strict in the starting value.
{-# INLINE foldr' #-}
foldr' :: (EntitySymbol -> b -> b) -> b -> SymbolSet -> b
foldr' fn z = IM.foldr' fn z . fromSymbolSet

-- ----------------------------------------------------------------------------

-- | /O(n*min(n,W))/. Convert a 'SymbolSet' to an 'IS.IntSet' (internal use).
{-# INLINE toIntSet #-}
toIntSet :: SymbolSet -> IS.IntSet
toIntSet = IS.fromList . IM.keys . fromSymbolSet

-- | /O(n*min(n,W))/. Create a 'SymbolSet' from a list of 'EntitySymbol's.
{-# INLINE fromListSS #-}
fromListSS :: [EntitySymbol] -> SymbolSet
fromListSS = SymbolSet . IM.fromList . map (\e -> (getKeyES e, e))

-- | /O(n)/. Convert a 'SymbolSet' to a list of 'EntitySymbol's. Note that the
-- result of 'toListSS' is ordered using the 'Ord' instance of by the key of
-- the unique of the elements, not the 'Ord' instance of the 'EntitySymbol's
-- themselves.
{-# INLINE toListSS #-}
toListSS :: SymbolSet -> [EntitySymbol]
toListSS = IM.elems . fromSymbolSet
