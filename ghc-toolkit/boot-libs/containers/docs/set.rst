Sets
====

.. highlight:: haskell

Sets allow you to store *unique*, *ordered* elements, providing efficient
insertion, lookups, deletions, and set operations. There are two implementations
provided by the ``containers`` package: :haddock:`/Data.Set` and
:haddock:`/Data.IntSet`. Use ``IntSet`` if you are storing, well... ``Int`` s.

::

    data Set element = ...

    data IntSet = ...

.. IMPORTANT::
   ``Set`` relies on the `element` type having instances of the ``Eq`` and
   ``Ord`` typeclass for its internal representation. These are already defined
   for builtin types, and if you are using your own data type you can use the
   `deriving
   <https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Deriving>`_
   mechanism.


All of these implementations are *immutable* which means that any update
functions do not modify the set that you passed in, they creates a new set. In
order to keep the changes you need to assign it to a new variable. For example::

    let s1 = Set.fromList ["a", "b"]
    let s2 = Set.delete "a" s1
    print s1
    > fromList ["a","b"]
    print s2
    > fromList ["b"]


Short Example
-------------

The following GHCi session shows some of the basic set functionality::

    import qualified Data.Set as Set

    let dataStructures = Set.fromList ["Set", "Map", "Graph", "Sequence"]

    -- Check if "Map" and "Trie" are in the set of data structures.
    Set.member "Map" dataStructures
    > True

    Set.member "Trie" dataStructures
    > False


    -- Add "Trie" to our original set of data structures.
    let moreDataStructures = Set.insert "Trie" dataStructures

    Set.member "Trie" moreDataStructures
    > True


    -- Remove "Graph" from our original set of data structures.
    let fewerDataStructures = Set.delete "Graph" dataStructures

    Set.toAscList fewerDataStructures
    > ["Map","Sequence","Set"]


    -- Create a new set and combine it with our original set.
    let unorderedDataStructures = Set.fromList ["HashSet", "HashMap"]

    Set.union dataStructures unorderedDataStructures
    > fromList ["Graph","HashMap","HashSet","Map","Sequence","Set"]


.. TIP:: You can use the `OverloadedLists
	 <https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists>`_ extension so
	 you don't need to write ``fromList [1, 2, 3]`` everywhere. Instead you
	 can just write ``[1, 2, 3]`` and if the function is expecting a set it
	 will be converted automatically! The code here will continue to use
	 ``fromList`` for clarity though.


Importing Set and IntSet
------------------------

When using ``Set`` or ``IntSet`` in a Haskell source file you should always use
a ``qualified`` import because these modules export names that clash with the
standard Prelude. You can import the type constructor and addional functions
that you care about unqualified.

::

    import Data.Set (Set, lookupMin, lookupMax)
    import qualified Data.Set as Set

    import Data.IntSet (IntSet)
    import qualified Data.IntSet as IntSet


Common API Functions
--------------------

.. TIP::
   All of these functions that work for ``Set`` will also work for ``IntSet``,
   which has the element type ``a`` specialized to ``Int``. Anywhere that you
   see ``Set Int`` you can replace it with ``IntSet``. This will speed up
   most operations tremendously (see `Performance`_) with the exception of
   ``size`` which is O(1) for ``Set`` and O(n) for ``IntSet``.

.. NOTE::
   ``fromList [some,list,elements]`` is how a ``Set`` is printed.


Construction and Conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an empty set
"""""""""""""""""""

::

    Set.empty :: Set a
    Set.empty = ...

:haddock_short:`/Data.Set#empty` creates a set with zero elements.

::

    Set.empty
    > fromList []

Create a set with one element (singleton)
"""""""""""""""""""""""""""""""""""""""""

::

    Set.singleton :: a -> Set a
    Set.singleton x = ...

:haddock_short:`/Data.Set#singleton` creates a set with a single element ``x`` in
it.

::

    Set.singleton "containers"
    > fromList ["containers"]

    Set.singleton 1
    > fromList [1]

Create a set from a list
""""""""""""""""""""""""

::

    Set.fromList :: Ord a => [a] -> Set a
    Set.fromList xs = ...

:haddock_short:`/Data.Set#fromList` creates a set containing the elements of the
list ``xs``. Since sets don't contain duplicates, if there are repeated elements
in the list they will only appear once.

::

    Set.fromList ["base", "containers", "QuickCheck"]
    > fromList ["QuickCheck","base","containers"]

    Set.fromList [1, 1, 2, 3, 4, 4, 5, 1]
    > fromList [1,2,3,4,5]

Create a list from a set
""""""""""""""""""""""""

::

    Set.toAscList, Set.toList, Set.elems :: Set a -> [a]
    Set.toAscList s = ...

:haddock_short:`/Data.Set#toAscList`, :haddock_short:`/Data.Set#toList`, and
:haddock_short:`/Data.Set#elems` return a list containing the elements of the set
:haddock_short:`/`s`` in *ascending* order.

.. NOTE::
   These all do the same thing; use ``toAscList`` because its name indicates the
   ordering.

::

    Set.toDescList :: Set a -> [a]
    Set.toDescList s = ...

:haddock_short:`/Data.Set#toDescList` returns a list containing the elements of
the set ``s`` in *descending* order.

::

    Set.toAscList (Set.fromList [0, 2, 4, 6])
    > [0,2,4,6]

    Set.toDescList (Set.fromList [0, 2, 4, 6]
    > [6,4,2,0]


Querying
^^^^^^^^

Check if an element is in a set (member)
""""""""""""""""""""""""""""""""""""""""

::

    Set.member :: Ord a => a -> Set a -> Bool
    Set.member x s = ...

:haddock_short:`/Data.Set#member` returns ``True`` if the element ``x`` is in the
set ``s``, ``False`` otherwise.

::

    Set.member 0 Set.empty
    > False

    Set.member 0 (Set.fromList [0, 2, 4, 6])
    > True

Check if a set is empty
"""""""""""""""""""""""

::

    Set.null :: Set a -> Bool
    Set.null s = ...

:haddock_short:`/Data.Set#null` returns ``True`` if the set ``s`` is empty,
``False`` otherwise.

::

    Set.null Set.empty
    > True

    Set.null (Set.fromList [0, 2, 4, 6])
    > False


The number of elements in a set
"""""""""""""""""""""""""""""""

::

    Set.size :: Set a -> Int
    Set.size s = ...

:haddock_short:`/Data.Set#size` returns the number of elements in the set ``s``.

::

    Set.size Set.empty
    > 0

    Set.size (Set.fromList [0, 2, 4, 6])
    > 4

Find the minimum/maximum element in a set
"""""""""""""""""""""""""""""""""""""""""

*Since version 0.5.9*

::

   lookupMin, lookupMax :: Set a -> Maybe a
   lookupMin s = ...
   lookupMax s = ...

:haddock_short:`/Data.Set#lookupMin` returns the minimum, or maximum
respectively, element of the set ``s``, or ``Nothing`` if the set is empty.

::

    Set.lookupMin Set.empty
    > Nothing

    Set.lookupMin (Set.fromList [0, 2, 4, 6])
    > Just 0

    Set.lookupMax (Set.fromList [0, 2, 4, 6])
    > Just 6

.. WARNING::
   Unless you're using an old version of ``containers`` **DO NOT** use
   ``Set.findMin`` or ``Set.findMax``. They are partial and throw a runtime
   error if the set is empty.

Modification
^^^^^^^^^^^^

Adding a new element to a set
"""""""""""""""""""""""""""""

::

    Set.insert :: Ord a => a -> Set a -> Set a
    Set.insert x s = ...

:haddock_short:`/Data.Set#insert` places the element ``x`` into the set ``s``,
replacing an existing equal element if it already exists.

::

    Set.insert 100 Set.empty
    > fromList [100]

    Set.insert 0 (Set.fromList [0, 2, 4, 6])
    > fromList [0,2,4,6]

Removing an element from a set
""""""""""""""""""""""""""""""

::

    Set.delete :: Ord a => a -> Set a -> Set a
    Set.delete x s = ...

:haddock_short:`/Data.Set#delete` the element ``x`` from the set ``s``. If it’s
not a member it leaves the set unchanged.

::

    Set.delete 0 (Set.fromList [0, 2, 4, 6])
    > fromList [2,4,6]

Filtering elements from a set
"""""""""""""""""""""""""""""

::

    Set.filter :: (a -> Bool) -> Set a -> Set a
    Set.filter predicate s = ...

:haddock_short:`/Data.Set#filter` produces a set consisting of all elements of
``s`` for which the `predicate`` returns ``True``.

::

    Set.filter (==0) (Set.fromList [0, 2, 4, 6])
    > fromList [0]


Set Operations
^^^^^^^^^^^^^^

Union
"""""

::

    Set.union :: Ord a => Set a -> Set a -> Set a
    Set.union l r = ...

:haddock_short:`/Data.Set#union` returns a set containing all elements that are
in either of the two sets ``l`` or ``r`` (`set union
<https://en.wikipedia.org/wiki/Union_(set_theory)>`_).

::

    Set.union Set.empty (Set.fromList [0, 2, 4, 6])
    > fromList [0,2,4,6]

    Set.union (Set.fromList [1, 3, 5, 7]) (Set.fromList [0, 2, 4, 6])
    > fromList [0,1,2,3,4,5,6,7]

Intersection
""""""""""""

::

    Set.intersection :: Ord a => Set a -> Set a -> Set a
    Set.intersection l r = ...

:haddock_short:`/Data.Set#intersection` returns a set the elements that are in
both sets ``l`` and ``r`` (`set intersection
<https://en.wikipedia.org/wiki/Intersection_(set_theory)>`_).

::

    Set.intersection Set.empty (Set.fromList [0, 2, 4, 6])
    > fromList []

    Set.intersection (Set.fromList [1, 3, 5, 7]) (Set.fromList [0, 2, 4, 6])
    > fromList []

    Set.intersection (Set.singleton 0) (Set.fromList [0, 2, 4, 6])
    > fromList [0]

Difference
""""""""""

::

    Set.difference :: Ord a => Set a -> Set a -> Set a
    Set.difference l r = ...

:haddock_short:`/Data.Set#difference` returns a set containing the elements that
are in the first set ``l`` but not the second set ``r`` (`set
difference/relative compliment
<https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement>`_).

::

    Set.difference (Set.fromList [0, 2, 4, 6]) Set.empty
    > fromList [0,2,4,6]

    Set.difference (Set.fromList [0, 2, 4, 6]) (Set.fromList [1, 3, 5, 7])
    > fromList [0,2,4,6]

    Set.difference (Set.fromList [0, 2, 4, 6]) (Set.singleton 0)
    > fromList [2,4,6]

Subset
""""""

::

    Set.isSubsetOf :: Ord a => Set a -> Set a -> Bool
    Set.isSubsetOf l r = ...

:haddock_short:`/Data.Set#isSubsetOf` returns ``True`` if all elements in the
first set ``l`` are also in the second set ``r`` (`subset
<https://en.wikipedia.org/wiki/Subset>`_).

.. NOTE::
   We use `infix notation
   <https://wiki.haskell.org/Infix_operator#Using_infix_functions_with_prefix_notation>`_
   so that it reads nicer. These are back-ticks (`), not single quotes (').

::

    Set.empty `Set.isSubsetOf` Set.empty
    > True

    Set.empty `Set.isSubsetOf` (Set.fromList [0, 2, 4, 6])
    > True

    (Set.singleton 0) `Set.isSubsetOf` (Set.fromList [0, 2, 4, 6])
    > True

    (Set.singleton 1) `Set.isSubsetOf` (Set.fromList [0, 2, 4, 6])
    > False


Serialization
-------------

The best way to serialize and deserialize sets is to use one of the many
libraries which already support serializing sets. :haddock:`binary`,
:haddock:`cereal`, and :haddock:`store` are some common libraries that people
use.

.. TIP::
   If you are writing custom serialization code use
   :haddock_short:`/Data.Set#fromDistinctAscList` (see
   `#405 <https://github.com/haskell/containers/issues/405>`_ for more info).

Performance
-----------

The API docs are annotated with the Big-*O* complexities of each of the set
operations. For benchmarks see the `haskell-perf/sets
<https://github.com/haskell-perf/sets>`_ page.


Looking for more?
-----------------

Didn't find what you're looking for? This tutorial only covered the most common
set functions, for a full list of functions see the
:haddock_short:`/Data.Set#Set` and :haddock_short:`/Data.IntSet#IntSet` API
documentation.
