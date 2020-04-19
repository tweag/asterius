{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Coerce
import Data.Data
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import GHC.Exts (IsList (..))
import Prelude hiding (null)

-- | A set of 'EntitySymbol's.
newtype SymbolSet = SymbolSet (IM.IntMap EntitySymbol)
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
empty = coerce (IM.empty @EntitySymbol)

-- | /O(1)/. A set of one element.
{-# INLINE singleton #-}
singleton :: EntitySymbol -> SymbolSet
singleton e = SymbolSet $ IM.singleton (getKeyES e) e

-- | /O(n+m)/. The union of two sets.
{-# INLINE union #-}
union :: SymbolSet -> SymbolSet -> SymbolSet
union = coerce (IM.union @EntitySymbol)

-- | The union of a list of sets.
{-# INLINE unions #-}
unions :: Foldable.Foldable f => f SymbolSet -> SymbolSet
unions = Foldable.foldl' union empty

-- | /O(1)/. Is the set empty?
{-# INLINE null #-}
null :: SymbolSet -> Bool
null = coerce (IM.null @EntitySymbol)

-- | /O(n+m)/. Difference between two sets.
{-# INLINE difference #-}
difference :: SymbolSet -> SymbolSet -> SymbolSet
difference = coerce (IM.difference @EntitySymbol @EntitySymbol)

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator. This is a strict variant: each application of the operator
-- is evaluated before using the result in the next application. This function
-- is strict in the starting value.
{-# INLINE foldr' #-}
foldr' :: forall b. (EntitySymbol -> b -> b) -> b -> SymbolSet -> b
foldr' = coerce (IM.foldr' @EntitySymbol @b)

-- ----------------------------------------------------------------------------

-- | /O(n*min(n,W))/. Convert a 'SymbolSet' to an 'IS.IntSet' (internal use).
{-# INLINE toIntSet #-}
toIntSet :: SymbolSet -> IS.IntSet
toIntSet = coerce (IM.keysSet @EntitySymbol)

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
toListSS = coerce (IM.elems @EntitySymbol)
