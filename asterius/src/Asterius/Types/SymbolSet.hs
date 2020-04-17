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
-- Internally it is represented as an 'IS.IntSet', containing they keys of the
-- uniques of the 'EntitySymbol's captured.
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
    fromIntSet,

    -- ** Lists
    fromList,
  )
where

import qualified Data.Foldable as Foldable
import Prelude hiding (null)
import Asterius.Types.EntitySymbol
import Data.Data
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as IS
import Unique

-- | A set of 'EntitySymbol's.
newtype SymbolSet = SymbolSet { fromSymbolSet :: IS.IntSet }
  deriving newtype (Eq, Semigroup, Monoid)
  deriving stock (Data)

instance Show SymbolSet where
  -- TODO: Problem. This is not the set of entity symbols, but the set of the
  -- keys of their uniques. Impossible to revert getUnique, so that makes me
  -- wonder whether we really need the Show instance.
  showsPrec p (SymbolSet s) = showParen (p > 10) $
    showString "fromList " . shows (IS.toList s)

-- TODO: There is a copy of this function in Asterius.Types.SymbolMap. See to
-- it that we only have one copy of this in a shared location.
{-# INLINE getKeyES #-}
getKeyES :: EntitySymbol -> IM.Key
getKeyES = getKey . getUnique

-- ----------------------------------------------------------------------------

-- | /O(1)/. The empty set.
{-# INLINE empty #-}
empty :: SymbolSet
empty = SymbolSet IS.empty

-- | /O(1)/. A set of one element.
{-# INLINE singleton #-}
singleton :: EntitySymbol -> SymbolSet
singleton = SymbolSet . IS.singleton . getKeyES

-- | /O(n+m)/. The union of two sets.
{-# INLINE union #-}
union :: SymbolSet -> SymbolSet -> SymbolSet
union (SymbolSet s1) (SymbolSet s2) = SymbolSet (IS.union s1 s2)

-- | The union of a list of sets.
unions :: Foldable.Foldable f => f SymbolSet -> SymbolSet
unions xs = Foldable.foldl' union empty xs

-- | /O(1)/. Is the set empty?
{-# INLINE null #-}
null :: SymbolSet -> Bool
null = IS.null . fromSymbolSet

-- | /O(n+m)/. Difference between two sets.
{-# INLINE difference #-}
difference :: SymbolSet -> SymbolSet -> SymbolSet
difference (SymbolSet s1) (SymbolSet s2) = SymbolSet (s1 `IS.difference` s2)

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator. This is a strict variant: each application of the operator
-- is evaluated before using the result in the next application. This function
-- is strict in the starting value.
foldr' :: (EntitySymbol -> b -> b) -> b -> SymbolSet -> b
foldr' = error "TODO"
  -- GEORGE: due to the variance of this function, it is not possible to
  -- implement: getKeyES is irreversible.
  --   foldr' :: (Key -> b -> b) -> b -> IntSet -> b
  -- We have to change the call site as well.

-- ----------------------------------------------------------------------------

-- /O(1)/. Convert a 'SymbolSet' to an 'IntSet'. TODO: Internal.
{-# INLINE toIntSet #-}
toIntSet :: SymbolSet -> IS.IntSet
toIntSet = fromSymbolSet

-- /O(1)/. Convert an 'IntSet' to a 'SymbolSet'. TODO: Internal.
{-# INLINE fromIntSet #-}
fromIntSet :: IS.IntSet -> SymbolSet
fromIntSet = SymbolSet

-- /O(n*min(n,W))/. Create a 'SymbolSet' from a list of 'EntitySymbol's.
{-# INLINE fromList #-}
fromList :: [EntitySymbol] -> SymbolSet
fromList = SymbolSet . IS.fromList . map getKeyES

