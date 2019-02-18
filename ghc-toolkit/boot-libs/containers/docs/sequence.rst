Sequences
=========

.. highlight:: haskell

Sequences allow you to store a finite number of sequential elements, providing
fast access to both ends of the sequence as well as efficient concatenation. The
``containers`` package provides the :haddock:`/Data.Sequence` module which
defines the ``Seq`` data type.


Short Example
-------------

The following GHCi session shows some of the basic sequence funcitonality::

    -- Import the Seq type and operators for combining sequences unqualified.
    -- Import the rest of the Sequence module qualified.
    import Data.Sequence (Seq(..), (<|), (|>), (><))
    import qualified Data.Sequence as Seq

    let nums = Seq.fromList [1, 2, 3]


    -- Put numbers on the front and back.
    0 <| nums
    > fromList [0,1,2,3]

    nums |> 4
    > fromList [1,2,3,4]


    -- Reverse a sequence
    Seq.reverse (Seq.fromList [0, 1, 2])
    > fromList [2,1,0]


    -- Put two sequences together.
    (Seq.fromList [-2, -1]) >< nums
    > fromList [-2,-1,0,1,2]


    -- Check if a sequence is empty and check the length.
    Seq.null nums
    > False

    Seq.length nums
    > 3


    -- Lookup an element at a certain index (since version 0.4.8).
    Seq.lookup 2 nums
    > Just 3

    -- Or the unsafe version, you MUST check length beforehand.
    Seq.index 2 nums
    > 3


    -- Map a function over a sequence (can use fmap or the infix function <$>).
    fmap show nums
    > fromList ["0","1","2"]

    show <$> nums
    > fromList ["0","1","2"]


    -- Fold a sequence into a summary value.
    foldr (+) 0 (Seq.fromList [0, 1, 2])
    > 3

.. TIP:: You can use the `OverloadedLists
	 <http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-lists>`_
	 extension so you don't need to write ``fromList [1, 2, 3]`` everywhere.
	 Instead you can just write ``[1, 2, 3]`` and if the function is
	 expecting a sequence it will be converted automatically! The code here
	 will continue to use ``fromList`` for clarity.


Importing Sequence
------------------

When using ``Sequence`` in a Haskell source file you should always use a
``qualified`` import becasue it exports names that clash with the standard
Prelude (you can import the type constructor and some operators on their own
though!).

::

    import Data.Sequence (Seq, (<|), (|>), (><))
    import qualified Data.Sequence as Seq


Common API Functions
--------------------

.. NOTE::
   ``fromList [some,sequence,elements]`` is how a ``Seq`` is printed.

Construction and Conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an empty sequence
""""""""""""""""""""""""

::

    Seq.empty :: Seq a
    Seq.empty = ...

:haddock_short:`/Data.Sequence#empty` creates a sequence with zero elements.

::

    Seq.empty
    > fromList []


Create a sequence with one element (singleton)
""""""""""""""""""""""""""""""""""""""""""""""

::

    Seq.singleton :: a -> Seq a
    Seq.singleton x = ...

:haddock_short:`/Data.Sequence#singleton` creates a sequence with the single
element ``x`` in it.

::

    Seq.singleton "containers"
    > fromList ["containers"]

    Seq.singleton 1
    > fromList [1]

Create a sequence with the same element repeated
""""""""""""""""""""""""""""""""""""""""""""""""

::

    Seq.replicate :: Int -> a -> Seq a
    Seq.replicate n x = ...

:haddock_short:`/Data.Sequence#replicate` creates a sequence with same element
``x`` repeated ``n`` times.

::

    Seq.replicate 0 "hi"
    > fromList []

    Seq.replicate 3 "hi"
    > fromList ["hi","hi","hi"]

Create a sequence from a list
"""""""""""""""""""""""""""""

::

    Seq.fromList :: [a] -> Seq a
    Seq.FromList xs = ...

:haddock_short:`/Data.Sequence#fromList` creates a sequence containing the
elements of the list ``xs``. Sequences allow duplicate so all elements will be
included in the order given.

::

    Seq.fromList ["base", "containers", "QuickCheck"]
    > fromList ["base","containers","QuickCheck"]

    Seq.fromList [0, 1, 1, 2, 3, 1]
    > fromList [0,1,1,2,3,1]

Adding to an existing sequence
""""""""""""""""""""""""""""""

::

    (<|) :: a -> Seq a -> Seq a
    x <| xs = ...

    (|>) :: Seq a -> a -> Seq a
    xs |> x = ...

    (><) :: Seq a -> Seq a -> Seq a
    l >< r = ...

- ``x <| xs`` places the element ``x`` at the beginning of the sequence ``xs``.

- ``xs |> x`` places the element ``x`` at the end of the sequence ``xs``.

- ``l >< r`` combines the two sequences ``l`` and ``r`` together.


Create a list from a sequence
"""""""""""""""""""""""""""""

::

    import qualified Data.Foldable as Foldable
    Foldable.toList :: Seq a -> [a]


There is no ``toList`` function in the Sequence module since it can be
`easily implemented <https://wiki.haskell.org/Foldable_and_Traversable>`_ with a
fold using ``Seq``'s `Foldable
<https://wiki.haskell.org/Typeclassopedia#Foldable>`_ instance.

::

    import qualified Data.Foldable as Foldable
    Foldable.toList (Seq.fromList ["base", "containers", "QuickCheck"])
    > ["base","containers","QuickCheck"]


Pattern Matching
^^^^^^^^^^^^^^^^

*Since 0.5.10*

Just like you can pattern match (aka. destructure) a list ``[a]``, you can do
the same with sequences. Let's first look at how we do this with lists::

    case [1, 2, 3] of
      [] -> "empty list"
      (x:xs) -> "first:" ++ show x ++ " rest:" ++ show xs
    > "first:1 rest:[2,3]"


Let's do the same thing with sequences!

::

    -- Imports the patterns to match on.
    import Data.Sequence (Seq (Empty, (:<|), (:|>)))

    case Seq.fromList [1, 2, 3] of
      Empty -> "empty sequence"
      x :<| xs -> "first:" ++ x ++ " rest:" ++ show xs
    > "first:1 rest:fromList [2,3]"

.. NOTE:: You can't copy/paste this into GHCi because it's multiple lines.

You can also take an element off the end::

    -- Imports the patterns to match on.
    import Data.Sequence (Seq (Empty, (:<|), (:|>)))

    case Seq.fromList [1, 2, 3] of
      Empty -> "empty sequence"
      xs :|> x -> "last element:" ++ show x
    > "last element:3"

Querying
^^^^^^^^

Check if a sequence is empty
""""""""""""""""""""""""""""

::

    Seq.null :: Seq a -> Bool
    Seq.null xs = ...

:haddock_short:`/Data.Sequence#null` returns ``True`` if the sequence ``xs`` is
empty, and ``False`` otherwise.

::

    Seq.null Seq.empty
    > True

    Seq.null (Seq.fromList [1, 2, 3])
    > False

The length/size of a sequence
"""""""""""""""""""""""""""""

::

    Seq.length :: Seq a -> Int
    Seq.length xs = ...

:haddock_short:`/Data.Sequence#length` returns the length of the sequence ``xs``.

::

    Seq.length Seq.empty
    > 0

    Seq.length (Seq.fromList [1, 2, 3])
    > 3

The element at a given index
""""""""""""""""""""""""""""

::

    Seq.lookup :: Int -> Seq a -> Maybe a
    Seq.lookup n xs = ...

    Seq.!? :: Seq a -> Int -> Maybe a
    xs !? n = ...

:haddock_short:`/Data.Sequence#lookup` returns the element at the position ``n``,
or ``Nothing`` if the index is out of bounds. :haddock_short:`/Data.Sequence#!?`
is simply a flipped version of ``lookup``.

.. NOTE::
   You may need to import ``!?`` qualified if you're using a ``Map``,
   ``IntMap``, or ``Vector`` in the same file because they all export the
   same operator.

::

    Seq.index :: Seq a -> Int -> a
    Seq.index xs n = ...

:haddock_short:`/Data.Sequence#index` returns the element at the given
position. It throws a runtime error if the index is out of bounds.

.. TIP::
   Use ``lookup``/``!?`` whenever you can and explicitly deal with the
   ``Nothing`` case.

::

    (Seq.fromList ["base", "containers"]) Seq.!? 0
    > Just "base"

    Seq.index 0 (Seq.fromList ["base", "containers"])
    > "base"

    (Seq.fromList ["base", "containers"]) Seq.!? 2
    > Nothing

    Seq.index (Seq.fromList ["base", "containers"]) 2
    > "*** Exception: index out of bounds

When working with functions that return a ``Maybe v``, use a `case expression
<https://en.wikibooks.org/wiki/Haskell/Control_structures#case_expressions>`_ to
deal with the ``Just`` or ``Nothing`` value::

   do
     let firstDependency = Seq.fromList ["base", "containers"] !? 0
     case firstDependency of
       Nothing -> print "Whoops! No dependencies!"
       Just dep -> print "The first dependency is " ++ dep


Modification
^^^^^^^^^^^^

Inserting an element
""""""""""""""""""""

::

    Seq.insertAt :: Int -> a -> Seq a -> Seq a
    Seq.insertAt i x xs = ...

:haddock_short:`/Data.Sequence#insertAt` inserts ``x`` into ``xs`` at the index
``i``, shifting the rest of the sequence over. If ``i`` is out of range then
``x`` will be inserted at the beginning or the end of the sequence as
appropriate.

::

    Seq.insertAt 0 "idris" (Seq.fromList ["haskell", "rust"])
    > fromList ["idris","haskell","rust"]

    Seq.insertAt (-10) "idris" (Seq.fromList ["haskell", "rust"])
    > fromList ["idris","haskell","rust"]

    Seq.insertAt 10 "idris" (Seq.fromList ["haskell", "rust"])
    > fromList ["haskell","rust","idris"]

See also `Adding to an existing sequence`_.

Delete an element
"""""""""""""""""

::

    Seq.deleteAt :: Int -> Seq a -> Seq a
    Seq.deleteAt i xs = ...

:haddock_short:`/Data.Sequence#deleteAt` removes the element of the sequence at
index ``i``. If the index is out of bounds then the original sequence is
returned.

::

    Seq.deleteAt 0 (Seq.fromList [0, 1, 2])
    > fromList [1,2]

    Seq.deleteAt 10 (Seq.fromList [0, 1, 2])
    > fromList [0,1,2]

Replace an element
""""""""""""""""""

::

    Seq.update :: Int -> a -> Seq a -> Seq a
    Seq.update i x xs = ...

:haddock_short:`/Data.Sequence#update` replaces the element at position ``i`` in
the sequence with ``x``. If the index is out of bounds then the original
sequence is returned.

::

    Seq.update 0 "hello" (Seq.fromList ["hi", "world", "!"])
    > fromList ["hello","world","!"]

    Seq.update 3 "OUTOFBOUNDS" (Seq.fromList ["hi", "world", "!"])
    > fromList ["hi","world","!"]

Adjust/modify an element
""""""""""""""""""""""""

*Since version 0.5.8*

::

    adjust' :: forall a. (a -> a) -> Int -> Seq a -> Seq a
    adjust' f i xs = ...

:haddock_short:`/Data.Sequence#adjust'` updates the element at position ``i`` in
the sequence by applying the function ``f`` to the existing element. If the
index is out of bounds then the original sequence is returned.

::

    Seq.adjust' (*10) 0 (Seq.fromList [1, 2, 3])
    > fromList [10,2,3]

    Seq.adjust' (*10) 3 (Seq.fromList [1, 2, 3])
    > fromList [1,2,3]

.. NOTE::
   If you're using an older version of containers which only has ``adjust``, be
   careful because it can lead to poor performance and space leaks (see
   :haddock_short:`/Data.Sequence#adjust` docs).

Modifying all elements
""""""""""""""""""""""

::

    fmap :: (a -> b) -> Seq a -> Seq b
    fmap f xs = ...

    Seq.mapWithIndex :: (Int -> a -> b) -> Seq a -> Seq b
    Seq.mapWithIndex f xs = ...

:haddock_short:`/Data.Sequence#fmap` transform each element of the sequence with
the function ``f``. ``fmap`` is provided by the `Functor
<https://wiki.haskell.org/Typeclassopedia#Functor>`_ instance for sequences and
can also be written infix using the ``<$>`` operator.

:haddock_short:`/Data.Sequence#mapWithIndex` allows you to do a similar
transformation but gives you the index that each element is at.

::

    fmap (*10) (Seq.fromList [1, 2, 3])
    -- = fromList [1*10, 2*10, 3*10]
    > fromList [10,20,30]

    (*10) <$> Seq.fromList [1, 2, 3]
    -- = fromList [1*10, 2*10, 3*10]
    > fromList [10,20,30]

    let myMapFunc index val = index * val

    Seq.mapWithIndex myMapFunc (Seq.fromList [1, 2, 3])
    -- = fromList [0*1, 1*2, 2*3]
    > fromList [0,2,6]


Sorting
^^^^^^^

::

    Seq.sort :: Ord a => Seq a -> Seq a
    Seq.sort xs = ...

:haddock_short:`/Data.Sequence#sort` the sequence ``xs`` using the ``Ord``
instance.

::

    Seq.sort (Seq.fromList ["x", "a", "c", "b"])
    > fromList ["a","b","c","x"]


Subsequences
^^^^^^^^^^^^

Take
""""

::

    Seq.take :: Int -> Seq a -> Seq a
    Seq.take n xs = ...

:haddock_short:`/Data.Sequence#take` returns the first ``n`` elements of the
sequence ``xs``. If the length of ``xs`` is less than ``n`` then all elements
are returned.

::

    Seq.take 0 (Seq.fromList [1, 2, 3])
    > fromList []

    Seq.take 2 (Seq.fromList [1, 2, 3])
    > fromList [1,2]

    Seq.take 5 (Seq.fromList [1, 2, 3])
    > fromList [1,2,3]

Drop
""""

::

    Seq.drop :: Int -> Seq a -> Seq a
    Seq.drop n xs = ...

:haddock_short:`/Data.Sequence#drop` the first ``n`` elements of the sequence
``xs``. If the length of ``xs`` is less than ``n`` then an empty sequence is
returned.

::

    Seq.drop 0 (Seq.fromList [1, 2, 3])
    > fromList [1,2,3]

    Seq.drop 2 (Seq.fromList [1, 2, 3])
    > fromList [3]

    Seq.drop 5 (Seq.fromList [1, 2, 3])
    > fromList []

Chunks
""""""

::

    Seq.chunksOf :: Int -> Seq a -> Seq (Seq a)
    Seq.chunksOf k xs = ...

:haddock_short:`/Data.Sequence#chunksOf` splits the sequence ``xs`` into chunks
of size ``k``. If the length of the sequence is not evenly divisible by ``k``
then the last chunk will have less than ``k`` elements.

.. WARNING::
   ``k`` can only be ``0`` when the sequence is empty, otherwise a runtime
   error is thrown.

::

    -- A chunk size of 0 can ONLY be given for an empty sequence.
    Seq.chunksOf 0 Seq.empty
    > fromList []

    Seq.chunksOf 1 (Seq.fromList [1, 2, 3])
    > fromList [fromList [1],fromList [2],fromList [3]]

    Seq.chunksOf 2 (Seq.fromList [1, 2, 3])
    > fromList [fromList [1,2],fromList [3]]

    Seq.chunksOf 5 (Seq.fromList [1, 2, 3])
    > fromList [fromList [1,2,3]]


Folding
^^^^^^^

::

    foldr :: (a -> b -> b) -> b -> Seq a -> b
    foldr f init xs = ...

    Seq.foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b
    Seq.foldrWithIndex f init xs = ...

:haddock_short:`/Data.Sequence#foldr` collapses the sequence into a summary
value by repeatedly applying ``f``. ``foldr`` is provided by the `Foldable
<https://wiki.haskell.org/Typeclassopedia#Foldable>`_ instance for
sequences. :haddock_short:`/Data.Sequence#foldWithIndex` gives you access to the
position in the sequence when transforming each element.

::

    foldr (+) 0 (Seq.fromList [1, 2, 3])
    -- = (1 + (2 + (3 + 0)))
    > 6

    let myFoldFunction index val accum = (index * val) + accum

    Seq.foldrWithIndex myFoldFunction 0 (Seq.fromList [1, 2, 3])
    -- = ((0*1) + ((1*2) + ((2*3) + 0)))
    > 8


Serialization
-------------

The best way to serialize and deserialize sequences is to use one of the many
libraries which already support serializing sequences. :haddock:`binary`,
:haddock:`cereal`, and :haddock:`store` are some common libraries that people
use.


Performance
-----------

The API docs are annotated with the Big-*O* complexities of each of the sequence
operations. For benchmarks see the `haskell-perf/sequences
<https://github.com/haskell-perf/sequences>`_ page.


Looking for more?
-----------------

Didn't find what you're looking for? This tutorial only covered the most common
sequence functions, for a full list of functions see the
:haddock:`/Data.Sequence` API documentation.
