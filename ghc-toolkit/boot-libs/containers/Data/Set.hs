{-# LANGUAGE CPP #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set
-- Copyright   :  (c) Daan Leijen 2002
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Sets
--
-- The @'Set' e@ type represents a set of elements of type @e@. Most operations
-- require that @e@ be an instance of the 'Ord' class. A 'Set' is strict in its
-- elements.
--
-- For a walkthrough of the most commonly used functions see the
-- <https://haskell-containers.readthedocs.io/en/latest/set.html sets introduction>.
--
-- Note that the implementation is generally /left-biased/. Functions that take
-- two sets as arguments and combine them, such as `union` and `intersection`,
-- prefer the entries in the first argument to those in the second. Of course,
-- this bias can only be observed when equality is an equivalence relation
-- instead of structural equality.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.Set (Set)
-- >  import qualified Data.Set as Set
--
--
-- == Warning
--
-- The size of the set must not exceed @maxBound::Int@. Violation of
-- this condition is not detected and if the size limit is exceeded, its
-- behaviour is undefined.
--
--
-- == Implementation
--
-- The implementation of 'Set' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--      Journal of Functional Programming 3(4):553-562, October 1993,
--      <http://www.swiss.ai.mit.edu/~adams/BB/>.
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--
--  Bounds for 'union', 'intersection', and 'difference' are as given
--  by
--
--    * Guy Blelloch, Daniel Ferizovic, and Yihan Sun,
--      \"/Just Join for Parallel Ordered Sets/\",
--      <https://arxiv.org/abs/1602.02120v3>.
--
-----------------------------------------------------------------------------

module Data.Set (
            -- * Set type
#if !defined(TESTING)
              Set          -- instance Eq,Ord,Show,Read,Data,Typeable
#else
              Set(..)
#endif

            -- * Construction
            , empty
            , singleton
            , fromList
            , fromAscList
            , fromDescList
            , fromDistinctAscList
            , fromDistinctDescList
            , powerSet

            -- * Insertion
            , insert

            -- * Deletion
            , delete

            -- * Query
            , member
            , notMember
            , lookupLT
            , lookupGT
            , lookupLE
            , lookupGE
            , S.null
            , size
            , isSubsetOf
            , isProperSubsetOf
            , disjoint

            -- * Combine
            , union
            , unions
            , difference
            , (\\)
            , intersection
            , cartesianProduct
            , disjointUnion

            -- * Filter
            , S.filter
            , takeWhileAntitone
            , dropWhileAntitone
            , spanAntitone
            , partition
            , split
            , splitMember
            , splitRoot

            -- * Indexed
            , lookupIndex
            , findIndex
            , elemAt
            , deleteAt
            , S.take
            , S.drop
            , S.splitAt

            -- * Map
            , S.map
            , mapMonotonic

            -- * Folds
            , S.foldr
            , S.foldl
            -- ** Strict folds
            , foldr'
            , foldl'
            -- ** Legacy folds
            , fold

            -- * Min\/Max
            , lookupMin
            , lookupMax
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , maxView
            , minView

            -- * Conversion

            -- ** List
            , elems
            , toList
            , toAscList
            , toDescList

            -- * Debugging
            , showTree
            , showTreeWith
            , valid

#if defined(TESTING)
            -- Internals (for testing)
            , bin
            , balanced
            , link
            , merge
#endif
            ) where

import Data.Set.Internal as S
