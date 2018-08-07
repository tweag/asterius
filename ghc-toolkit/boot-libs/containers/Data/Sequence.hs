{-# LANGUAGE CPP #-}
#ifdef __HADDOCK_VERSION__
{-# OPTIONS_GHC -Wno-unused-imports #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence
-- Copyright   :  (c) Ross Paterson 2005
--                (c) Louis Wasserman 2009
--                (c) Bertram Felgenhauer, David Feuer, Ross Paterson, and
--                    Milan Straka 2014
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = Finite sequences
--
-- The @'Seq' a@ type represents a finite sequence of values of
-- type @a@.
--
-- Sequences generally behave very much like lists.
--
-- * The class instances for sequences are all based very closely on those for
-- lists.
--
-- * Many functions in this module have the same names as functions in
-- the "Prelude" or in "Data.List". In almost all cases, these functions
-- behave analogously. For example, 'filter' filters a sequence in exactly the
-- same way that @"Prelude".'Prelude.filter'@ filters a list. The only major
-- exception is the 'lookup' function, which is based on the function by
-- that name in "Data.IntMap" rather than the one in "Prelude".
--
-- There are two major differences between sequences and lists:
--
-- * Sequences support a wider variety of efficient operations than
-- do lists. Notably, they offer
--
--     * Constant-time access to both the front and the rear with
--     '<|', '|>', 'viewl', 'viewr'. For recent GHC versions, this can
--     be done more conveniently using the bidirectional patterns 'Empty',
--     ':<|', and ':|>'. See the detailed explanation in the \"Pattern synonyms\"
--     section.
--     * Logarithmic-time concatenation with '><'
--     * Logarithmic-time splitting with 'splitAt', 'take' and 'drop'
--     * Logarithmic-time access to any element with
--     'lookup', '!?', 'index', 'insertAt', 'deleteAt', 'adjust'', and 'update'
--
--   Note that sequences are typically /slower/ than lists when using only
--   operations for which they have the same big-\(O\) complexity: sequences
--   make rather mediocre stacks!
--
-- * Whereas lists can be either finite or infinite, sequences are
-- always finite. As a result, a sequence is strict in its
-- length. Ignoring efficiency, you can imagine that 'Seq' is defined
--
--     @ data Seq a = Empty | a :<| !(Seq a) @
--
--     This means that many operations on sequences are stricter than
--     those on lists. For example,
--
--     @ (1 : undefined) !! 0 = 1 @
--
--     but
--
--     @ (1 :<| undefined) ``index`` 0 = undefined @
--
-- Sequences may also be compared to immutable
-- [arrays](https://hackage.haskell.org/package/array)
-- or [vectors](https://hackage.haskell.org/package/vector).
-- Like these structures, sequences support fast indexing,
-- although not as fast. But editing an immutable array or vector,
-- or combining it with another, generally requires copying the
-- entire structure; sequences generally avoid that, copying only
-- the portion that has changed.
--
-- == Detailed performance information
--
-- An amortized running time is given for each operation, with /n/ referring
-- to the length of the sequence and /i/ being the integral index used by
-- some operations. These bounds hold even in a persistent (shared) setting.
--
-- Despite sequences being structurally strict from a semantic standpoint,
-- they are in fact implemented using laziness internally. As a result,
-- many operations can be performed /incrementally/, producing their results
-- as they are demanded. This greatly improves performance in some cases. These
-- functions include
--
-- * The 'Functor' methods 'fmap' and '<$', along with 'mapWithIndex'
-- * The 'Applicative' methods '<*>', '*>', and '<*'
-- * The zips: 'zipWith', 'zip', etc.
-- * 'heads' and 'tails'
-- * 'fromFunction', 'replicate', 'intersperse', and 'cycleTaking'
-- * 'reverse'
-- * 'chunksOf'
--
-- Note that the 'Monad' method, '>>=', is not particularly lazy. It will
-- take time proportional to the sum of the logarithms of the individual
-- result sequences to produce anything whatsoever.
--
-- Several functions take special advantage of sharing to produce
-- results using much less time and memory than one might expect. These
-- are documented individually for functions, but also include the
-- methods '<$' and '*>', each of which take time and space proportional
-- to the logarithm of the size of the result.
--
-- == Warning
--
-- The size of a 'Seq' must not exceed @maxBound::Int@. Violation
-- of this condition is not detected and if the size limit is exceeded, the
-- behaviour of the sequence is undefined. This is unlikely to occur in most
-- applications, but some care may be required when using '><', '<*>', '*>', or
-- '>>', particularly repeatedly and particularly in combination with
-- 'replicate' or 'fromFunction'.
--
-- == Implementation
--
-- The implementation uses 2-3 finger trees annotated with sizes,
-- as described in section 4.2 of
--
--    * Ralf Hinze and Ross Paterson,
--      [\"Finger trees: a simple general-purpose data structure\"]
--      (http://staff.city.ac.uk/~ross/papers/FingerTree.html),
--      /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--
-----------------------------------------------------------------------------


module Data.Sequence (
    -- * Finite sequences
#if defined(DEFINE_PATTERN_SYNONYMS)
    Seq (Empty, (:<|), (:|>)),
    -- $patterns
#else
    Seq,
#endif
    -- * Construction
    empty,          -- :: Seq a
    singleton,      -- :: a -> Seq a
    (<|),           -- :: a -> Seq a -> Seq a
    (|>),           -- :: Seq a -> a -> Seq a
    (><),           -- :: Seq a -> Seq a -> Seq a
    fromList,       -- :: [a] -> Seq a
    fromFunction,   -- :: Int -> (Int -> a) -> Seq a
    fromArray,      -- :: Ix i => Array i a -> Seq a
    -- ** Repetition
    replicate,      -- :: Int -> a -> Seq a
    replicateA,     -- :: Applicative f => Int -> f a -> f (Seq a)
    replicateM,     -- :: Applicative m => Int -> m a -> m (Seq a)
    cycleTaking,    -- :: Int -> Seq a -> Seq a
    -- ** Iterative construction
    iterateN,       -- :: Int -> (a -> a) -> a -> Seq a
    unfoldr,        -- :: (b -> Maybe (a, b)) -> b -> Seq a
    unfoldl,        -- :: (b -> Maybe (b, a)) -> b -> Seq a
    -- * Deconstruction
    -- | Additional functions for deconstructing sequences are available
    -- via the 'Foldable' instance of 'Seq'.

    -- ** Queries
    null,           -- :: Seq a -> Bool
    length,         -- :: Seq a -> Int
    -- ** Views
    ViewL(..),
    viewl,          -- :: Seq a -> ViewL a
    ViewR(..),
    viewr,          -- :: Seq a -> ViewR a
    -- * Scans
    scanl,          -- :: (a -> b -> a) -> a -> Seq b -> Seq a
    scanl1,         -- :: (a -> a -> a) -> Seq a -> Seq a
    scanr,          -- :: (a -> b -> b) -> b -> Seq a -> Seq b
    scanr1,         -- :: (a -> a -> a) -> Seq a -> Seq a
    -- * Sublists
    tails,          -- :: Seq a -> Seq (Seq a)
    inits,          -- :: Seq a -> Seq (Seq a)
    chunksOf,       -- :: Int -> Seq a -> Seq (Seq a)
    -- ** Sequential searches
    takeWhileL,     -- :: (a -> Bool) -> Seq a -> Seq a
    takeWhileR,     -- :: (a -> Bool) -> Seq a -> Seq a
    dropWhileL,     -- :: (a -> Bool) -> Seq a -> Seq a
    dropWhileR,     -- :: (a -> Bool) -> Seq a -> Seq a
    spanl,          -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    spanr,          -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    breakl,         -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    breakr,         -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    partition,      -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    filter,         -- :: (a -> Bool) -> Seq a -> Seq a
    -- * Sorting
    sort,           -- :: Ord a => Seq a -> Seq a
    sortBy,         -- :: (a -> a -> Ordering) -> Seq a -> Seq a
    sortOn,         -- :: Ord b => (a -> b) -> Seq a -> Seq a
    unstableSort,   -- :: Ord a => Seq a -> Seq a
    unstableSortBy, -- :: (a -> a -> Ordering) -> Seq a -> Seq a
    unstableSortOn, -- :: Ord b => (a -> b) -> Seq a -> Seq a
    -- * Indexing
    lookup,         -- :: Int -> Seq a -> Maybe a
    (!?),           -- :: Seq a -> Int -> Maybe a
    index,          -- :: Seq a -> Int -> a
    adjust,         -- :: (a -> a) -> Int -> Seq a -> Seq a
    adjust',        -- :: (a -> a) -> Int -> Seq a -> Seq a
    update,         -- :: Int -> a -> Seq a -> Seq a
    take,           -- :: Int -> Seq a -> Seq a
    drop,           -- :: Int -> Seq a -> Seq a
    insertAt,       -- :: Int -> a -> Seq a -> Seq a
    deleteAt,       -- :: Int -> Seq a -> Seq a
    splitAt,        -- :: Int -> Seq a -> (Seq a, Seq a)
    -- ** Indexing with predicates
    -- | These functions perform sequential searches from the left
    -- or right ends of the sequence, returning indices of matching
    -- elements.
    elemIndexL,     -- :: Eq a => a -> Seq a -> Maybe Int
    elemIndicesL,   -- :: Eq a => a -> Seq a -> [Int]
    elemIndexR,     -- :: Eq a => a -> Seq a -> Maybe Int
    elemIndicesR,   -- :: Eq a => a -> Seq a -> [Int]
    findIndexL,     -- :: (a -> Bool) -> Seq a -> Maybe Int
    findIndicesL,   -- :: (a -> Bool) -> Seq a -> [Int]
    findIndexR,     -- :: (a -> Bool) -> Seq a -> Maybe Int
    findIndicesR,   -- :: (a -> Bool) -> Seq a -> [Int]
    -- * Folds
    -- | General folds are available via the 'Foldable' instance of 'Seq'.
    foldMapWithIndex, -- :: Monoid m => (Int -> a -> m) -> Seq a -> m
    foldlWithIndex, -- :: (b -> Int -> a -> b) -> b -> Seq a -> b
    foldrWithIndex, -- :: (Int -> a -> b -> b) -> b -> Seq a -> b
    -- * Transformations
    mapWithIndex,   -- :: (Int -> a -> b) -> Seq a -> Seq b
    traverseWithIndex, -- :: Applicative f => (Int -> a -> f b) -> Seq a -> f (Seq b)
    reverse,        -- :: Seq a -> Seq a
    intersperse,    -- :: a -> Seq a -> Seq a
    -- ** Zips and unzip
    zip,            -- :: Seq a -> Seq b -> Seq (a, b)
    zipWith,        -- :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
    zip3,           -- :: Seq a -> Seq b -> Seq c -> Seq (a, b, c)
    zipWith3,       -- :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
    zip4,           -- :: Seq a -> Seq b -> Seq c -> Seq d -> Seq (a, b, c, d)
    zipWith4,       -- :: (a -> b -> c -> d -> e) -> Seq a -> Seq b -> Seq c -> Seq d -> Seq e
    unzip,          -- :: Seq (a, b) -> (Seq a, Seq b)
    unzipWith       -- :: (a -> (b, c)) -> Seq a -> (Seq b, Seq c)
    ) where

import Data.Sequence.Internal
import Data.Sequence.Internal.Sorting
import Prelude ()
#ifdef __HADDOCK_VERSION__
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))
import Data.Functor (Functor (..))
#endif

{- $patterns

== Pattern synonyms

Much like lists can be constructed and matched using the
@:@ and @[]@ constructors, sequences can be constructed and
matched using the 'Empty', ':<|', and ':|>' pattern synonyms.

=== Note

These patterns are only available with GHC version 8.0 or later,
and version 8.2 works better with them. When writing for such recent
versions of GHC, the patterns can be used in place of 'empty',
'<|', '|>', 'viewl', and 'viewr'.

=== __Pattern synonym examples__

Import the patterns:

@
import Data.Sequence (Seq (..))
@

Look at the first three elements of a sequence

@
getFirst3 :: Seq a -> Maybe (a,a,a)
getFirst3 (x1 :<| x2 :<| x3 :<| _xs) = Just (x1,x2,x3)
getFirst3 _ = Nothing
@

@
\> getFirst3 ('fromList' [1,2,3,4]) = Just (1,2,3)
\> getFirst3 ('fromList' [1,2]) = Nothing
@

Move the last two elements from the end of the first list
onto the beginning of the second one.

@
shift2Right :: Seq a -> Seq a -> (Seq a, Seq a)
shift2Right Empty ys = (Empty, ys)
shift2Right (Empty :|> x) ys = (Empty, x :<| ys)
shift2Right (xs :|> x1 :|> x2) = (xs, x1 :<| x2 :<| ys)
@

@
\> shift2Right ('fromList' []) ('fromList' [10]) = ('fromList' [], 'fromList' [10])
\> shift2Right ('fromList' [9]) ('fromList' [10]) = ('fromList' [], 'fromList' [9,10])
\> shift2Right ('fromList' [8,9]) ('fromList' [10]) = ('fromList' [], 'fromList' [8,9,10])
\> shift2Right ('fromList' [7,8,9]) ('fromList' [10]) = ('fromList' [7], 'fromList' [8,9,10])
@
-}
