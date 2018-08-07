{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Strict
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Maps (strict interface)
--
-- The @'Map' k v@ type represents a finite map (sometimes called a dictionary)
-- from keys of type @k@ to values of type @v@.
--
-- Each function in this module is careful to force values before installing
-- them in a 'Map'. This is usually more efficient when laziness is not
-- necessary. When laziness /is/ required, use the functions in "Data.Map.Lazy".
--
-- In particular, the functions in this module obey the following law:
--
--  - If all values stored in all maps in the arguments are in WHNF, then all
--    values stored in all maps in the results will be in WHNF once those maps
--    are evaluated.
--
-- When deciding if this is the correct data structure to use, consider:
--
-- * If you are using 'Int' keys, you will get much better performance for most
-- operations using "Data.IntMap.Strict".
--
-- * If you don't care about ordering, consider use @Data.HashMap.Strict@ from the
-- <https://hackage.haskell.org/package/unordered-containers unordered-containers>
-- package instead.
--
-- For a walkthrough of the most commonly used functions see the
-- <https://haskell-containers.readthedocs.io/en/latest/map.html maps introduction>.
--
-- This module is intended to be imported qualified, to avoid name clashes with
-- Prelude functions:
--
-- > import qualified Data.Map.Strict as Map
--
-- Note that the implementation is generally /left-biased/. Functions that take
-- two maps as arguments and combine them, such as `union` and `intersection`,
-- prefer the values in the first argument to those in the second.
--
--
-- == Detailed performance information
--
-- The amortized running time is given for each operation, with /n/ referring to
-- the number of entries in the map.
--
-- Benchmarks comparing "Data.Map.Strict" with other dictionary implementations
-- can be found at https://github.com/haskell-perf/dictionaries.
--
--
-- == Warning
--
-- The size of a 'Map' must not exceed @maxBound::Int@. Violation of this
-- condition is not detected and if the size limit is exceeded, its behaviour is
-- undefined.
--
-- The 'Map' type is shared between the lazy and strict modules, meaning that
-- the same 'Map' value can be passed to functions in both modules. This means
-- that the 'Functor', 'Traversable' and 'Data' instances are the same as for
-- the "Data.Map.Lazy" module, so if they are used the resulting maps may contain
-- suspended values (thunks).
--
--
-- == Implementation
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--     Journal of Functional Programming 3(4):553-562, October 1993,
--     <http://www.swiss.ai.mit.edu/~adams/BB/>.
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
--
-----------------------------------------------------------------------------

-- See the notes at the beginning of Data.Map.Internal.

module Data.Map.Strict
    (
    -- * Map type
    Map              -- instance Eq,Show,Read

    -- * Construction
    , empty
    , singleton
    , fromSet

    -- ** From Unordered Lists
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** From Ascending Lists
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- ** From Descending Lists
    , fromDescList
    , fromDescListWith
    , fromDescListWithKey
    , fromDistinctDescList

    -- * Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- * Deletion\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    , alterF

    -- * Query
    -- ** Lookup
    , lookup
    , (!?)
    , (!)
    , findWithDefault
    , member
    , notMember
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE

    -- ** Size
    , null
    , size

    -- * Combine

    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , (\\)
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** General combining functions
    -- | See "Data.Map.Merge.Strict"

    -- ** Deprecated general combining function

    , mergeWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , traverseMaybeWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs
    , keysSet

    -- ** Lists
    , toList

    -- ** Ordered lists
    , toAscList
    , toDescList

    -- * Filter
    , filter
    , filterWithKey
    , restrictKeys
    , withoutKeys
    , partition
    , partitionWithKey

    , takeWhileAntitone
    , dropWhileAntitone
    , spanAntitone

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Indexed
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt
    , take
    , drop
    , splitAt

    -- * Min\/Max
    , lookupMin
    , lookupMax
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey

    -- * Debugging
#ifdef __GLASGOW_HASKELL__
    , showTree
    , showTreeWith
#endif
    , valid
    ) where

import Data.Map.Strict.Internal
import Prelude ()
