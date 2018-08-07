# Sorting

## Unstable Sorting

Unstable sorting is performed by a heap sort implementation based on
pairing heaps.  Because the internal structure of sequences is quite
varied, it is difficult to get blocks of elements of roughly the same
length, which would improve merge sort performance.  Pairing heaps,
on the other hand, are relatively resistant to the effects of merging
heaps of wildly different sizes, as guaranteed by its amortized
constant-time merge operation.  Moreover, extensive use of SpecConstr
transformations can be done on pairing heaps, especially when we're
only constructing them to immediately be unrolled.

On purely random sequences of length 50000, with no RTS options,
I get the following statistics, in which heapsort is about 42.5%
faster:  (all comparisons done with -O2)

Times (ms)        |  min  |  mean  | +/-sd | median |  max
------------------|-------|--------|-------|--------|-------
to/from list:     |103.802| 108.572|  7.487| 106.436|143.339
unstable heapsort:| 60.686|  62.968|  4.275|  61.187| 79.151

Heapsort, it would seem, is less of a memory hog than Data.List.sortBy.
The gap is narrowed when more memory is available, but heapsort still
wins, 15% faster, with +RTS -H128m:

Times (ms)        |  min  | mean | +/-sd | median |  max
------------------|-------|------|-------|--------|-------
to/from list:     | 42.692|45.074|  2.596|  44.600| 56.601
unstable heapsort:| 37.100|38.344|  3.043|  37.715| 55.526

In addition, on strictly increasing sequences the gap is even wider
than normal; heapsort is 68.5% faster with no RTS options:

Times (ms)        |  min  | mean | +/-sd | median |  max
------------------|-------|------|-------|--------|-------
to/from list:     | 52.236|53.574|  1.987|  53.034| 62.098
unstable heapsort:| 16.433|16.919|  0.931|  16.681| 21.622

This may be attributed to the elegant nature of the pairing heap.

wasserman.louis@gmail.com, 7/20/09

----------------------------------------------------------------------

David Feuer wrote an unstable sort for arbitrary traversables,
https://www.reddit.com/r/haskell/comments/63a4ea/fast_total_sorting_of_arbitrary_traversable/,
which turned out to be competitive with the unstable sort here.
Feuer suggested that this indicated some room to improve on the
unstable sort.

The two main improvements to the original function are a specialize
pragma on replicateA (this gives a 26.5% speedup) and removal of the
intermediate list (a further 11.7% speedup). These numbers are all on
purely random sequences of length 50000:

Times (ms)       | min | est | max |std dev|  r²
-----------------|-----|-----|-----|-------|-----
to/from list:    |70.90|72.44|75.07|  2.224|0.998
7/20/09 heapsort:|59.84|61.44|63.08|  1.554|0.998
7/20/09 w/pragma:|44.22|45.14|46.25|  1.631|0.998
4/30/17 heapsort:|38.21|39.86|40.87|  1.203|0.996

It should also be noted that Data.List.sortBy has become
significantly quicker. Data.List.sortBy also now recognizes strictly
increasing sequences, making it much quicker for that case:

Times (ms)       | min | est | max |std dev|  r²
-----------------|-----|-----|-----|-------|-----
to/from list:    |7.140|7.351|7.634|  0.335|0.993
7/20/09 heapsort:|19.52|19.78|20.13|  0.445|0.999
7/20/09 w/pragma:|8.050|8.271|8.514|  0.357|0.995
4/30/17 heapsort:|7.240|7.612|7.925|  0.389|0.991

Another happy result of the specialization of 'replicateA' is that
the stable sort seems to speed up by 10-20%, and 'iterateN' looks
like it's about three times as fast.

mail@doisinkidney.com, 4/30/17

## Stable Sorting

Stable sorting was previously accomplished by converting to a list,
applying Data.List.sort, and rebuilding the sequence. Data.List.sort is
designed to maximize laziness, which doesn't apply for Data.Sequence,
and it can't take advantage of the structure of the finger tree. As a
result, simply tagging each element with its position, then applying
the unstable sort (using the tag to discriminate between elements for
which the comparator is equal) is faster. The current implementation
doesn't use the actual `unstableSort`: to perform the building of the
queue and tagging in one pass, a specialized version is used.

The algorithm is effectively the same as the unstable sorts, except
the queue is constructed while giving each element a tag.

It's quicker than the old implementation (which used Data.List.sort)
in the general case (all times are on sequences of length 50000):

Times (ms)          | min | est | max |std dev|  r²
--------------------|-----|-----|-----|-------|-----
to/from list:       |64.23|64.50|64.81|  0.432|1.000
1/11/18 stable heap:|38.87|39.40|40.09|  0.457|0.999

Slightly slower in the case of already sorted lists:

Times (ms)          | min | est | max |std dev|  r²
--------------------|-----|-----|-----|-------|-----
to/from list:       |6.806|6.861|6.901|  0.234|1.000
1/11/18 stable heap:|8.211|8.268|8.328|  0.111|1.000

And quicker in the case of lists sorted in reverse:

Times (ms)          | min | est | max |std dev|  r²
--------------------|-----|-----|-----|-------|-----
to/from list:       |26.79|28.34|30.55|  1.219|0.988
1/11/18 stable heap:|9.405|10.13|10.91|  0.670|0.977

Interestingly, the stable sort is now competitive with the unstable:

Times (ms)| min | est | max |std dev|  r²
----------|-----|-----|-----|-------|-----
unstable: |34.71|35.10|35.38|  0.845|1.000
stable:   |38.84|39.22|39.59|  0.564|0.999

And even beats it in the case of already-sorted lists:

Times (ms)| min | est | max |std dev|  r²
----------|-----|-----|-----|-------|-----
unstable: |8.457|8.499|8.536|  0.069|1.000
stable:   |8.160|8.230|8.334|  0.158|0.999

mail@doisinkidney.com, 1/11/18

## sortOn Functions

The `sortOn` and `unstableSortOn` functions perform the Schwartzian transform, however instead of the following implementation:

```haskell
sortOn f = fmap snd . sortBy (conparing fst) . fmap (\x -> (f x, x))
```

The `fmap`s are fused manually with the creation of the queue, avoiding the two extra traversals. It still suffers a slowdown of roughly 20%:

Times (ms)     | min | est | max |std dev|  r²
---------------|-----|-----|-----|-------|-----
unstableSortOn |43.68|44.58|45.95|  0.677|0.999
unstableSort   |36.55|37.43|38.33|  0.533|0.999
sortOn         |48.22|49.03|50.09|  1.110|0.998
sort           |41.81|43.17|45.31|  1.172|0.996

The heaps are also specialized to avoid the creation of a tuple.

## Other Heaps

The pairing heap seems to particularly suit the structure of the finger tree, as other heaps have not managed to beat it. Specifically, when compared to a skew heap:

```haskell
unstableSortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
unstableSortBy cmp (Seq xs) =
    execState (replicateA (size xs) (popMin cmp)) (toSkew cmp (Seq xs))

data Skew a = Nil | Br a !(Skew a) !(Skew a)

popMin :: (e -> e -> Ordering) -> State (Skew e) e
popMin cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (Br x ls rs) = (mergeSkew cmp ls rs, x)

toSkew :: (e -> e -> Ordering) -> Seq e -> Skew e
toSkew cmp (Seq xs') = toSkewTree cmp (\(Elem a) -> Br a Nil Nil) xs'
  where
    toSkewTree :: (b -> b -> Ordering) -> (a -> Skew b) -> FingerTree a -> Skew b
    toSkewTree _ _ EmptyT = Nil
    toSkewTree _ f (Single xs) = f xs
    toSkewTree cmp f (Deep n pr m sf) = pr' <+> sf' <+> m'
      where
        pr' = toSkewDigit cmp f pr
        sf' = toSkewDigit cmp f sf
        m' = toSkewTree cmp (toSkewNode cmp f) m
        (<+>) = mergeSkew cmp
    toSkewDigit :: (b -> b -> Ordering) -> (a -> Skew b) -> Digit a -> Skew b
    toSkewDigit cmp f dig =
        case dig of
            One a -> f a
            Two a b -> f a <+> f b
            Three a b c -> f a <+> f b <+> f c
            Four a b c d -> (f a <+> f b) <+> (f c <+> f d)
      where
        (<+>) = mergeSkew cmp
    toSkewNode cmp f node =
        case node of
            Node2 _ a b -> f a <+> f b
            Node3 _ a b c -> f a <+> f b <+> f c
      where
        (<+>) = mergeSkew cmp

mergeSkew :: (a -> a -> Ordering) -> Skew a -> Skew a -> Skew a
mergeSkew cmp Nil ys = ys
mergeSkew cmp xs Nil = xs
mergeSkew cmp h1@(Br x lx rx) h2@(Br y ly ry)
  | cmp x y == GT = Br y (mergeSkew cmp h1 ry) ly
  | otherwise     = Br x (mergeSkew cmp h2 rx) lx
```

The pairing heap implementation is faster in every aspect:

```
benchmarking 1000000/unsorted/pairing
time                 2.005 s    (NaN s .. 2.102 s)
                     1.000 R²   (0.998 R² .. 1.000 R²)
mean                 2.069 s    (2.060 s .. 2.075 s)
std dev              9.340 ms   (0.0 s .. 10.67 ms)
variance introduced by outliers: 19% (moderately inflated)
             
benchmarking 1000000/unsorted/skew
time                 2.042 s    (1.637 s .. 2.267 s)
                     0.995 R²   (0.990 R² .. NaN R²)
mean                 2.165 s    (2.065 s .. 2.217 s)
std dev              87.10 ms   (0.0 s .. 91.26 ms)
variance introduced by outliers: 19% (moderately inflated)
             
benchmarking 1000000/ascending/pairing
time                 191.4 ms   (187.8 ms .. 193.5 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 197.0 ms   (194.7 ms .. 200.0 ms)
std dev              3.221 ms   (2.441 ms .. 3.924 ms)
variance introduced by outliers: 14% (moderately inflated)
             
benchmarking 1000000/ascending/skew
time                 232.3 ms   (227.0 ms .. 238.9 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 233.9 ms   (230.6 ms .. 236.2 ms)
std dev              3.678 ms   (2.790 ms .. 4.777 ms)
variance introduced by outliers: 14% (moderately inflated)
             
benchmarking 1000000/descending/pairing
time                 204.6 ms   (190.2 ms .. 214.1 ms)
                     0.998 R²   (0.991 R² .. 1.000 R²)
mean                 208.4 ms   (204.1 ms .. 210.6 ms)
std dev              4.051 ms   (1.299 ms .. 5.288 ms)
variance introduced by outliers: 14% (moderately inflated)
             
benchmarking 1000000/descending/skew
time                 229.9 ms   (212.7 ms .. 240.1 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 238.8 ms   (231.3 ms .. 241.4 ms)
std dev              5.006 ms   (269.0 μs .. 6.151 ms)
variance introduced by outliers: 16% (moderately inflated)
```

