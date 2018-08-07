{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- This contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
-- = Description
--
-- This module provides the various sorting implementations for
-- "Data.Sequence". Further notes are available in the file sorting.md
-- (in this directory).

module Data.Sequence.Internal.Sorting
  (
   -- * Sort Functions
   sort
  ,sortBy
  ,sortOn
  ,unstableSort
  ,unstableSortBy
  ,unstableSortOn
  ,
   -- * Heaps
   -- $heaps
   Queue(..)
  ,QList(..)
  ,IndexedQueue(..)
  ,IQList(..)
  ,TaggedQueue(..)
  ,TQList(..)
  ,IndexedTaggedQueue(..)
  ,ITQList(..)
  ,
   -- * Merges
   -- $merges
   mergeQ
  ,mergeIQ
  ,mergeTQ
  ,mergeITQ
  ,
   -- * popMin
   -- $popMin
   popMinQ
  ,popMinIQ
  ,popMinTQ
  ,popMinITQ
  ,
   -- * Building
   -- $building
   buildQ
  ,buildIQ
  ,buildTQ
  ,buildITQ
  ,
   -- * Special folds
   -- $folds
   foldToMaybeTree
  ,foldToMaybeWithIndexTree)
  where

import Data.Sequence.Internal
       (Elem(..), Seq(..), Node(..), Digit(..), Sized(..), FingerTree(..),
        replicateA, foldDigit, foldNode, foldWithIndexDigit,
        foldWithIndexNode)
import Utils.Containers.Internal.State (State(..), execState)
-- | \( O(n \log n) \).  'sort' sorts the specified 'Seq' by the natural
-- ordering of its elements.  The sort is stable.  If stability is not
-- required, 'unstableSort' can be slightly faster.
--
-- @since 0.3.0
sort :: Ord a => Seq a -> Seq a
sort = sortBy compare

-- | \( O(n \log n) \).  'sortBy' sorts the specified 'Seq' according to the
-- specified comparator.  The sort is stable.  If stability is not required,
-- 'unstableSortBy' can be slightly faster.
--
-- @since 0.3.0
sortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
sortBy cmp (Seq xs) =
    maybe
        (Seq EmptyT)
        (execState (replicateA (size xs) (State (popMinIQ cmp))))
        (buildIQ cmp (\s (Elem x) -> IQ s x IQNil) 0 xs)

-- | \( O(n \log n) \). 'sortOn' sorts the specified 'Seq' by comparing
-- the results of a key function applied to each element. @'sortOn' f@ is
-- equivalent to @'sortBy' ('compare' ``Data.Function.on`` f)@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list. This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
--
-- An example of using 'sortOn' might be to sort a 'Seq' of strings
-- according to their length:
--
-- > sortOn length (fromList ["alligator", "monkey", "zebra"]) == fromList ["zebra", "monkey", "alligator"]
--
-- If, instead, 'sortBy' had been used, 'length' would be evaluated on
-- every comparison, giving \( O(n \log n) \) evaluations, rather than
-- \( O(n) \).
--
-- If @f@ is very cheap (for example a record selector, or 'fst'),
-- @'sortBy' ('compare' ``Data.Function.on`` f)@ will be faster than
-- @'sortOn' f@.
--
-- @since 0.5.11
sortOn :: Ord b => (a -> b) -> Seq a -> Seq a
sortOn f (Seq xs) =
    maybe
       (Seq EmptyT)
       (execState (replicateA (size xs) (State (popMinITQ compare))))
       (buildITQ compare (\s (Elem x) -> ITQ s (f x) x ITQNil) 0 xs)

-- | \( O(n \log n) \).  'unstableSort' sorts the specified 'Seq' by
-- the natural ordering of its elements, but the sort is not stable.
-- This algorithm is frequently faster and uses less memory than 'sort'.

-- Notes on the implementation and choice of heap are available in
-- the file sorting.md (in this directory).
--
-- @since 0.3.0
unstableSort :: Ord a => Seq a -> Seq a
unstableSort = unstableSortBy compare

-- | \( O(n \log n) \).  A generalization of 'unstableSort', 'unstableSortBy'
-- takes an arbitrary comparator and sorts the specified sequence.
-- The sort is not stable.  This algorithm is frequently faster and
-- uses less memory than 'sortBy'.
--
-- @since 0.3.0
unstableSortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
unstableSortBy cmp (Seq xs) =
    maybe
        (Seq EmptyT)
        (execState (replicateA (size xs) (State (popMinQ cmp))))
        (buildQ cmp (\(Elem x) -> Q x Nil) xs)

-- | \( O(n \log n) \). 'unstableSortOn' sorts the specified 'Seq' by
-- comparing the results of a key function applied to each element.
-- @'unstableSortOn' f@ is equivalent to @'unstableSortBy' ('compare' ``Data.Function.on`` f)@,
-- but has the performance advantage of only evaluating @f@ once for each
-- element in the input list. This is called the
-- decorate-sort-undecorate paradigm, or Schwartzian transform.
--
-- An example of using 'unstableSortOn' might be to sort a 'Seq' of strings
-- according to their length:
--
-- > unstableSortOn length (fromList ["alligator", "monkey", "zebra"]) == fromList ["zebra", "monkey", "alligator"]
--
-- If, instead, 'unstableSortBy' had been used, 'length' would be evaluated on
-- every comparison, giving \( O(n \log n) \) evaluations, rather than
-- \( O(n) \).
--
-- If @f@ is very cheap (for example a record selector, or 'fst'),
-- @'unstableSortBy' ('compare' ``Data.Function.on`` f)@ will be faster than
-- @'unstableSortOn' f@.
--
-- @since 0.5.11
unstableSortOn :: Ord b => (a -> b) -> Seq a -> Seq a
unstableSortOn f (Seq xs) =
    maybe
       (Seq EmptyT)
       (execState (replicateA (size xs) (State (popMinTQ compare))))
       (buildTQ compare (\(Elem x) -> TQ (f x) x TQNil) xs)

------------------------------------------------------------------------
-- $heaps
--
-- The following are definitions for various specialized pairing heaps.
--
-- All of the heaps are defined to be non-empty, which speeds up the
-- merge functions.
------------------------------------------------------------------------

-- | A simple pairing heap.
data Queue e = Q !e (QList e)
data QList e
    = Nil
    | QCons {-# UNPACK #-} !(Queue e)
            (QList e)

-- | A pairing heap tagged with the original position of elements,
-- to allow for stable sorting.
data IndexedQueue e =
    IQ {-# UNPACK #-} !Int !e (IQList e)
data IQList e
    = IQNil
    | IQCons {-# UNPACK #-} !(IndexedQueue e)
             (IQList e)

-- | A pairing heap tagged with some key for sorting elements, for use
-- in 'unstableSortOn'.
data TaggedQueue a b =
    TQ !a b (TQList a b)
data TQList a b
    = TQNil
    | TQCons {-# UNPACK #-} !(TaggedQueue a b)
             (TQList a b)

-- | A pairing heap tagged with both a key and the original position
-- of its elements, for use in 'sortOn'.
data IndexedTaggedQueue e a =
    ITQ {-# UNPACK #-} !Int !e a (ITQList e a)
data ITQList e a
    = ITQNil
    | ITQCons {-# UNPACK #-} !(IndexedTaggedQueue e a)
              (ITQList e a)

infixr 8 `ITQCons`, `TQCons`, `QCons`, `IQCons`

------------------------------------------------------------------------
-- $merges
--
-- The following are definitions for "merge" for each of the heaps
-- above. Each takes a comparison function which is used to order the
-- elements.
------------------------------------------------------------------------

-- | 'mergeQ' merges two 'Queue's.
mergeQ :: (a -> a -> Ordering) -> Queue a -> Queue a -> Queue a
mergeQ cmp q1@(Q x1 ts1) q2@(Q x2 ts2)
  | cmp x1 x2 == GT = Q x2 (q1 `QCons` ts2)
  | otherwise       = Q x1 (q2 `QCons` ts1)

-- | 'mergeTQ' merges two 'TaggedQueue's, based on the tag value.
mergeTQ :: (a -> a -> Ordering)
        -> TaggedQueue a b
        -> TaggedQueue a b
        -> TaggedQueue a b
mergeTQ cmp q1@(TQ x1 y1 ts1) q2@(TQ x2 y2 ts2)
  | cmp x1 x2 == GT = TQ x2 y2 (q1 `TQCons` ts2)
  | otherwise       = TQ x1 y1 (q2 `TQCons` ts1)

-- | 'mergeIQ' merges two 'IndexedQueue's, taking into account the
-- original position of the elements.
mergeIQ :: (a -> a -> Ordering)
        -> IndexedQueue a
        -> IndexedQueue a
        -> IndexedQueue a
mergeIQ cmp q1@(IQ i1 x1 ts1) q2@(IQ i2 x2 ts2) =
    case cmp x1 x2 of
        LT -> IQ i1 x1 (q2 `IQCons` ts1)
        EQ | i1 <= i2 -> IQ i1 x1 (q2 `IQCons` ts1)
        _ -> IQ i2 x2 (q1 `IQCons` ts2)

-- | 'mergeITQ' merges two 'IndexedTaggedQueue's, based on the tag
-- value, taking into account the original position of the elements.
mergeITQ
    :: (a -> a -> Ordering)
    -> IndexedTaggedQueue a b
    -> IndexedTaggedQueue a b
    -> IndexedTaggedQueue a b
mergeITQ cmp q1@(ITQ i1 x1 y1 ts1) q2@(ITQ i2 x2 y2 ts2) =
    case cmp x1 x2 of
        LT -> ITQ i1 x1 y1 (q2 `ITQCons` ts1)
        EQ | i1 <= i2 -> ITQ i1 x1 y1 (q2 `ITQCons` ts1)
        _ -> ITQ i2 x2 y2 (q1 `ITQCons` ts2)

------------------------------------------------------------------------
-- $popMin
--
-- The following are definitions for @popMin@, a function which
-- constructs a stateful action which pops the smallest element from the
-- queue, where "smallest" is according to the supplied comparison
-- function.
--
-- All of the functions fail on an empty queue.
--
-- Each of these functions is structured something like this:
--
-- @popMinQ cmp (Q x ts) = (mergeQs ts, x)@
--
-- The reason the call to @mergeQs@ is lazy is that it will be bottom
-- for the last element in the queue, preventing us from evaluating the
-- fully sorted sequence.
------------------------------------------------------------------------

-- | Pop the smallest element from the queue, using the supplied
-- comparator.
popMinQ :: (e -> e -> Ordering) -> Queue e -> (Queue e, e)
popMinQ cmp (Q x xs) = (mergeQs xs, x)
  where
    mergeQs (t `QCons` Nil) = t
    mergeQs (t1 `QCons` t2 `QCons` Nil) = t1 <+> t2
    mergeQs (t1 `QCons` t2 `QCons` ts) = (t1 <+> t2) <+> mergeQs ts
    mergeQs Nil = error "popMinQ: tried to pop from empty queue"
    (<+>) = mergeQ cmp

-- | Pop the smallest element from the queue, using the supplied
-- comparator, deferring to the item's original position when the
-- comparator returns 'EQ'.
popMinIQ :: (e -> e -> Ordering) -> IndexedQueue e -> (IndexedQueue e, e)
popMinIQ cmp (IQ _ x xs) = (mergeQs xs, x)
  where
    mergeQs (t `IQCons` IQNil) = t
    mergeQs (t1 `IQCons` t2 `IQCons` IQNil) = t1 <+> t2
    mergeQs (t1 `IQCons` t2 `IQCons` ts) = (t1 <+> t2) <+> mergeQs ts
    mergeQs IQNil = error "popMinQ: tried to pop from empty queue"
    (<+>) = mergeIQ cmp

-- | Pop the smallest element from the queue, using the supplied
-- comparator on the tag.
popMinTQ :: (a -> a -> Ordering) -> TaggedQueue a b -> (TaggedQueue a b, b)
popMinTQ cmp (TQ _ x xs) = (mergeQs xs, x)
  where
    mergeQs (t `TQCons` TQNil) = t
    mergeQs (t1 `TQCons` t2 `TQCons` TQNil) = t1 <+> t2
    mergeQs (t1 `TQCons` t2 `TQCons` ts) = (t1 <+> t2) <+> mergeQs ts
    mergeQs TQNil = error "popMinQ: tried to pop from empty queue"
    (<+>) = mergeTQ cmp

-- | Pop the smallest element from the queue, using the supplied
-- comparator on the tag, deferring to the item's original position
-- when the comparator returns 'EQ'.
popMinITQ :: (e -> e -> Ordering)
          -> IndexedTaggedQueue e b
          -> (IndexedTaggedQueue e b, b)
popMinITQ cmp (ITQ _ _ x xs) = (mergeQs xs, x)
  where
    mergeQs (t `ITQCons` ITQNil) = t
    mergeQs (t1 `ITQCons` t2 `ITQCons` ITQNil) = t1 <+> t2
    mergeQs (t1 `ITQCons` t2 `ITQCons` ts) = (t1 <+> t2) <+> mergeQs ts
    mergeQs ITQNil = error "popMinQ: tried to pop from empty queue"
    (<+>) = mergeITQ cmp

------------------------------------------------------------------------
-- $building
--
-- The following are definitions for functions to build queues, given a
-- comparison function.
------------------------------------------------------------------------

buildQ :: (b -> b -> Ordering) -> (a -> Queue b) -> FingerTree a -> Maybe (Queue b)
buildQ cmp = foldToMaybeTree (mergeQ cmp)

buildIQ
    :: (b -> b -> Ordering)
    -> (Int -> Elem y -> IndexedQueue b)
    -> Int
    -> FingerTree (Elem y)
    -> Maybe (IndexedQueue b)
buildIQ cmp = foldToMaybeWithIndexTree (mergeIQ cmp)

buildTQ
    :: (b -> b -> Ordering)
    -> (a -> TaggedQueue b c)
    -> FingerTree a
    -> Maybe (TaggedQueue b c)
buildTQ cmp = foldToMaybeTree (mergeTQ cmp)

buildITQ
    :: (b -> b -> Ordering)
    -> (Int -> Elem y -> IndexedTaggedQueue b c)
    -> Int
    -> FingerTree (Elem y)
    -> Maybe (IndexedTaggedQueue b c)
buildITQ cmp = foldToMaybeWithIndexTree (mergeITQ cmp)

------------------------------------------------------------------------
-- $folds
--
-- A big part of what makes the heaps fast is that they're non empty,
-- so the merge function can avoid an extra case match. To take
-- advantage of this, though, we need specialized versions of 'foldMap'
-- and 'Data.Sequence.foldMapWithIndex', which can alternate between
-- calling the faster semigroup-like merge when folding over non empty
-- structures (like 'Node' and 'Digit'), and the
-- 'Data.Semirgroup.Option'-like mappend, when folding over structures
-- which can be empty (like 'FingerTree').
------------------------------------------------------------------------

-- | A 'foldMap'-like function, specialized to the
-- 'Data.Semigroup.Option' monoid, which takes advantage of the
-- internal structure of 'Seq' to avoid wrapping in 'Maybe' at certain
-- points.
foldToMaybeTree :: (b -> b -> b) -> (a -> b) -> FingerTree a -> Maybe b
foldToMaybeTree _ _ EmptyT = Nothing
foldToMaybeTree _ f (Single xs) = Just (f xs)
foldToMaybeTree (<+>) f (Deep _ pr m sf) =
    Just (maybe (pr' <+> sf') ((pr' <+> sf') <+>) m')
  where
    pr' = foldDigit (<+>) f pr
    sf' = foldDigit (<+>) f sf
    m' = foldToMaybeTree (<+>) (foldNode (<+>) f) m
{-# INLINE foldToMaybeTree #-}

-- | A 'foldMapWithIndex'-like function, specialized to the
-- 'Data.Semigroup.Option' monoid, which takes advantage of the
-- internal structure of 'Seq' to avoid wrapping in 'Maybe' at certain
-- points.
foldToMaybeWithIndexTree :: (b -> b -> b)
                         -> (Int -> Elem y -> b)
                         -> Int
                         -> FingerTree (Elem y)
                         -> Maybe b
foldToMaybeWithIndexTree = foldToMaybeWithIndexTree'
  where
    {-# SPECIALISE foldToMaybeWithIndexTree' :: (b -> b -> b) -> (Int -> Elem y -> b) -> Int -> FingerTree (Elem y) -> Maybe b #-}
    {-# SPECIALISE foldToMaybeWithIndexTree' :: (b -> b -> b) -> (Int -> Node y -> b) -> Int -> FingerTree (Node y) -> Maybe b #-}
    foldToMaybeWithIndexTree'
        :: Sized a
        => (b -> b -> b) -> (Int -> a -> b) -> Int -> FingerTree a -> Maybe b
    foldToMaybeWithIndexTree' _ _ !_s EmptyT = Nothing
    foldToMaybeWithIndexTree' _ f s (Single xs) = Just (f s xs)
    foldToMaybeWithIndexTree' (<+>) f s (Deep _ pr m sf) =
        Just (maybe (pr' <+> sf') ((pr' <+> sf') <+>) m')
      where
        pr' = digit (<+>) f s pr
        sf' = digit (<+>) f sPsprm sf
        m' = foldToMaybeWithIndexTree' (<+>) (node (<+>) f) sPspr m
        !sPspr = s + size pr
        !sPsprm = sPspr + size m
    {-# SPECIALISE digit :: (b -> b -> b) -> (Int -> Elem y -> b) -> Int -> Digit (Elem y) -> b #-}
    {-# SPECIALISE digit :: (b -> b -> b) -> (Int -> Node y -> b) -> Int -> Digit (Node y) -> b #-}
    digit
        :: Sized a
        => (b -> b -> b) -> (Int -> a -> b) -> Int -> Digit a -> b
    digit = foldWithIndexDigit
    {-# SPECIALISE node :: (b -> b -> b) -> (Int -> Elem y -> b) -> Int -> Node (Elem y) -> b #-}
    {-# SPECIALISE node :: (b -> b -> b) -> (Int -> Node y -> b) -> Int -> Node (Node y) -> b #-}
    node
        :: Sized a
        => (b -> b -> b) -> (Int -> a -> b) -> Int -> Node a -> b
    node = foldWithIndexNode
{-# INLINE foldToMaybeWithIndexTree #-}
