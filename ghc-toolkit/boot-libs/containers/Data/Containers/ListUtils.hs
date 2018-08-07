{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Containers.ListUtils
-- Copyright   :  (c) Gershom Bazerman 2018
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This module provides efficient containers-based functions on the list type.
-----------------------------------------------------------------------------

module Data.Containers.ListUtils (
       nubOrd,
       nubOrdOn,
       nubInt,
       nubIntOn
       ) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
#ifdef __GLASGOW_HASKELL__
import GHC.Exts ( build )
#endif

-- *** Ord-based nubbing ***


-- | \( O(n \log n \). The @nubOrd@ function removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element. By using a
-- 'Set' internally it has better asymptotics than the standard 'Data.List.nub'
-- function.
--
-- ==== Strictness
--
-- @nubOrd@ is strict in the elements of the list.
--
-- ==== Efficiency note
--
-- When applicable, it is almost always better to use 'nubInt' or 'nubIntOn' instead
-- of this function. For example, the best way to nub a list of characters is
--
-- @ nubIntOn fromEnum xs @
nubOrd :: Ord a => [a] -> [a]
nubOrd = nubOrdOn id
{-# INLINE nubOrd #-}

-- | The @nubOrdOn@ function behaves just like 'nubOrd' except it performs
-- comparisons not on the original datatype, but a user-specified projection
-- from that datatype.
--
-- ==== Strictness
--
-- @nubOrdOn@ is strict in the values of the function applied to the
-- elements of the list.
nubOrdOn :: Ord b => (a -> b) -> [a] -> [a]
-- For some reason we need to write an explicit lambda here to allow this
-- to inline when only applied to a function.
nubOrdOn f = \xs -> nubOrdOnExcluding f Set.empty xs
{-# INLINE nubOrdOn #-}

-- Splitting nubOrdOn like this means that we don't have to worry about
-- matching specifically on Set.empty in the rewrite-back rule.
nubOrdOnExcluding :: Ord b => (a -> b) -> Set b -> [a] -> [a]
nubOrdOnExcluding f = go
  where
    go _ [] = []
    go s (x:xs)
      | fx `Set.member` s = go s xs
      | otherwise = x : go (Set.insert fx s) xs
      where !fx = f x

#ifdef __GLASGOW_HASKELL__
-- We want this inlinable to specialize to the necessary Ord instance.
{-# INLINABLE [1] nubOrdOnExcluding #-}

{-# RULES
-- Rewrite to a fusible form.
"nubOrdOn" [~1] forall f as s. nubOrdOnExcluding  f s as =
  build (\c n -> foldr (nubOrdOnFB f c) (constNubOn n) as s)

-- Rewrite back to a plain form
"nubOrdOnList" [1] forall f as s.
    foldr (nubOrdOnFB f (:)) (constNubOn []) as s =
       nubOrdOnExcluding f s as
 #-}

nubOrdOnFB :: Ord b
           => (a -> b)
           -> (a -> r -> r)
           -> a
           -> (Set b -> r)
           -> Set b
           -> r
nubOrdOnFB f c x r s
  | fx `Set.member` s = r s
  | otherwise = x `c` r (Set.insert fx s)
  where !fx = f x
{-# INLINABLE [0] nubOrdOnFB #-}

constNubOn :: a -> b -> a
constNubOn x _ = x
{-# INLINE [0] constNubOn #-}
#endif


-- *** Int-based nubbing ***


-- | \( O(n \min(n,W)) \). The @nubInt@ function removes duplicate 'Int'
-- values from a list. In particular, it keeps only the first occurrence
-- of each element. By using an 'IntSet' internally, it attains better
-- asymptotics than the standard 'Data.List.nub' function.
--
-- See also 'nubIntOn', a more widely applicable generalization.
--
-- ==== Strictness
--
-- @nubInt@ is strict in the elements of the list.
nubInt :: [Int] -> [Int]
nubInt = nubIntOn id
{-# INLINE nubInt #-}

-- | The @nubIntOn@ function behaves just like 'nubInt' except it performs
-- comparisons not on the original datatype, but a user-specified projection
-- from that datatype.
--
-- ==== Strictness
--
-- @nubIntOn@ is strict in the values of the function applied to the
-- elements of the list.
nubIntOn :: (a -> Int) -> [a] -> [a]
-- For some reason we need to write an explicit lambda here to allow this
-- to inline when only applied to a function.
nubIntOn f = \xs -> nubIntOnExcluding f IntSet.empty xs
{-# INLINE nubIntOn #-}

-- Splitting nubIntOn like this means that we don't have to worry about
-- matching specifically on IntSet.empty in the rewrite-back rule.
nubIntOnExcluding :: (a -> Int) -> IntSet -> [a] -> [a]
nubIntOnExcluding f = go
  where
    go _ [] = []
    go s (x:xs)
      | fx `IntSet.member` s = go s xs
      | otherwise = x : go (IntSet.insert fx s) xs
      where !fx = f x

#ifdef __GLASGOW_HASKELL__
-- We don't mark this INLINABLE because it doesn't seem obviously useful
-- to inline it anywhere; the elements the function operates on are actually
-- pulled from a list and installed in a list; the situation is very different
-- when fusion occurs. In this case, we let GHC make the call.
{-# NOINLINE [1] nubIntOnExcluding #-}

{-# RULES
"nubIntOn" [~1] forall f as s. nubIntOnExcluding  f s as =
  build (\c n -> foldr (nubIntOnFB f c) (constNubOn n) as s)
"nubIntOnList" [1] forall f as s. foldr (nubIntOnFB f (:)) (constNubOn []) as s =
  nubIntOnExcluding f s as
 #-}

nubIntOnFB :: (a -> Int)
           -> (a -> r -> r)
           -> a
           -> (IntSet -> r)
           -> IntSet
           -> r
nubIntOnFB f c x r s
  | fx `IntSet.member` s = r s
  | otherwise = x `c` r (IntSet.insert fx s)
  where !fx = f x
{-# INLINABLE [0] nubIntOnFB #-}
#endif
