Maps
====

.. highlight:: haskell

Maps (sometimes referred to as dictionaries in other languages) allow you to
store associations between *unique keys* and *values*. There are three
implementations provided by the ``containers`` package:
:haddock:`/Data.Map.Strict`, :haddock:`/Data.Map.Lazy`, and
:haddock:`/Data.IntMap`. You almost never want the lazy version so use
``Data.Map.Strict``, or if your keys are ``Int`` use ``Data.IntMap``.

::

    data Map k v = ...

    data IntMap v = ...

.. IMPORTANT::
   ``Map`` relies on the key type ``k`` having instances of the ``Eq`` and
   ``Ord`` typeclass for its internal representation. These are already defined
   for builtin types, and if you are using your own data type you can use the
   `deriving
   <https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Deriving>`_
   mechanism.

All of these implementations are *immutable* which means that any update
functions do not modify the map that you passed in, they creates a new map. In
order to keep the changes you need to assign it to a new variable. For example::

    let m1 = Map.fromList [("a", 1), ("b", 2)]
    let m2 = Map.delete "a" m1
    print m1
    > fromList [("a",1),("b",2)]
    print m2
    > fromList [("b",2)]


Short Example
-------------

The following GHCi session shows some of the basic map functionality::

    import qualified Data.Map.Strict as Map

    let nums = Map.fromList [(1,"one"), (2,"two"), (3,"three")]

    -- Get the English word for the number 3 and 4.
    Map.lookup 3 nums
    > Just "three"

    Map.lookup 4 nums
    > Nothing


    -- Add (4, "four") to our original map.
    let moreNums = Map.insert 4 "four" nums

    Map.member moreNums 4
    > True


    -- Remove the entry for 1 from our original map.
    let fewerNums = Map.delete 1 nums

    Map.toAscList fewerNums
    > [(2,"two"),(3,"three")]


    -- Create a new map and combine it with our original map.
    -- fromList is right-biased: if a key is repeated the rightmost value is taken.
    let newNums = Map.fromList [(3,"new three"), (4,"new four"), (4,"newer four")]

    -- union is left-biased: if a key occurs more than once the value from the
    -- left map is taken.
    Map.union newNums nums
    > fromList [(1,"one"),(2,"two"),(3,"new three"),(4,"newer four")]

.. TIP:: You can use the `OverloadedLists
	 <https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists>`_ extension so
	 you don't need to write ``fromList [1, 2, 3]`` everywhere; instead you
	 can just write ``[1, 2, 3]`` and if the function is expecting a map it
	 will be converted automatically! The code here will continue to use
	 ``fromList`` for clarity though.


Importing Map and IntMap
------------------------

When using ``Map`` or ``IntMap`` in a Haskell source file you should always use
a ``qualified`` import because these modules export names that clash with the
standard Prelude (you can import the type constructor on its own though!). You
should also import ``Prelude`` and hide ``lookup`` because if you accidentally
leave off the ``Map.`` qualifier you'll get confusing type errors. You can
always import any specific identifiers you want unqualified. Most of the time,
that will include the type constructor (``Map``).

::

    import Prelude hiding (lookup)

    import Data.Map.Strict (Map)
    import qualified Data.Map.Strict as Map

    import Data.IntMap (IntMap)
    import qualified Data.IntMap.Strict as IntMap


Common API Functions
--------------------

.. TIP::
   All of these functions that work for ``Map`` will also work for ``IntMap``,
   which has the key type ``k`` specialized to ``Int``. Anywhere that you
   see ``Map Int v`` you can replace it with ``IntMap v``. This will speed up
   most operations tremendously (see `Performance`_) with the exception of
   ``size`` which is O(1) for ``Map`` and O(n) for ``IntMap``.

.. NOTE::
   A ``Map`` is printed as an association list preceeded by ``fromList``. For
   example, it might look like ``fromList [(Key1,True),(Key2,False)]``.


Construction and Conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an empty map
"""""""""""""""""""

::

    Map.empty :: Map k v
    Map.empty = ...

:haddock_short:`/Data.Map.Strict#empty` creates a map without any entries.

::

    Map.empty
    > fromList []

Create a map with one entry (singleton)
"""""""""""""""""""""""""""""""""""""""

::

    Map.singleton :: k -> v -> Map k v
    Map.singleton key value = ...

:haddock_short:`/Data.Map.Strict#singleton` creates a map with a single
``(key,value)`` entry in it.

::

    Map.singleton 1 "one"
    > fromList [(1,"one")]

    Map.singleton "containers" ["base"]
    > fromList [("containers",["base"])]

Create a map from a list
""""""""""""""""""""""""

::

    Map.fromList :: Ord k => [(k, v)] -> Map k v
    Map.fromList xs = ...

:haddock_short:`/Data.Map.Strict#fromList` creates a map containing the entries
of the list ``xs`` where the keys comes from the first entries of the pairs and
the values from the second. If the same key appears more than once then the last
value is taken.

::

    Map.fromList []
    > fromList []

    Map.fromList [(1,"uno"), (1,"one"), (2,"two"), (3,"three")]
    > fromList [(1,"one"),(2,"two"),(3,"three")]

There's another incredibly useful function for constructing a map from a list::

    Map.fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map.Map k a
    Map.fromListWith f xs = ...

:haddock_short:`/Data.Map.Strict#fromListWith` allows you to build a map from a
list ``xs`` with repeated keys, where ``f`` is used to "combine" (or "choose")
values with the same key.

::

    -- Build a map from a list, but only keep the largest value for each key.
    Map.fromListWith max [("a", 2), ("a", 1), ("b", 2)]
    > fromList [("a",2),("b",2)]

    -- Build a histogram from a list of elements.
    Map.fromListWith (+) (map (\x -> (x, 1)) ["a", "a", "b", "c", "c", "c"])
    > fromList [("a",2),("b",1),("c",3)]

    -- Build a map from a list, combining the string values for the same key.
    Map.fromListWith (++) [(1, "a"), (1, "b"), (2, "x"), (2, "y")]
    > fromList [(1,"ba"),(2,"yx")]



Create a list from a map
""""""""""""""""""""""""

::

    Map.toAscList, Map.toList, Map.assocs :: Map k v -> [(k, v)]
    Map.toAscList m = ...

.. NOTE::
   These all do the same thing; use ``toAscList`` because its name indicates
   the ordering.

.. NOTE::
   ``Map.toList`` is **not** the same as ``Foldable.toList``; the latter is
   equivalent to ``elems``, although is rarely useful for maps. In general, use
   ``toAscList``.

:haddock_short:`/Data.Map.Strict#toAscList`,
:haddock_short:`/Data.Map.Strict#toList`, and
:haddock_short:`/Data.Map.Strict#assocs` returns a list containing the (key,
value) pairs in the map ``m`` in *ascending* key order.

::

    Map.toDescList :: Map k v -> [(k, v)]
    Map.toDescList m = ...

:haddock_short:`/Data.Map.Strict#toDescList` returns a list containing the (key,
value) pairs in the map ``m`` in *descending* key order.

::

    Map.toAscList (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > [(1,"one"),(2,"two"),(3,"three")]

    Map.toDescList (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > [(3,"three"),(2,"two"),(1,"one")]


Querying
^^^^^^^^

Lookup an entry in the map (lookup)
"""""""""""""""""""""""""""""""""""

::

    Map.lookup :: Ord k => k -> Map k v -> Maybe v
    Map.lookup key m = ...

    Map.!? :: Ord k => Map k v -> k -> Maybe v
    Map.!? m key = ...

:haddock_short:`/Data.Map.Strict#lookup` the value corresponding to the given
``key``, returns ``Nothing`` if the key is not present; the ``!?`` operator
(*since 0.5.10*) is a flipped version of ``lookup`` and can often be imported
unqualified.


If you want to provide a default value if the key doesn't exist you can do:

::

    import Data.Maybe (fromMaybe)

    -- fromMaybe :: a -> Maybe a -> a
    fromMaybe defaultValue (lookup k m)

For example::

    import Data.Map.Strict ((!?))
    import Data.Maybe (fromMaybe)

    Map.lookup 1 Map.empty
    > Nothing

    Map.lookup 1 (Map.fromList [(1,"one"),(2,"two"),(3,"three")])
    > Just "one"

    > (Map.fromList [(1,"one"),(2,"two"),(3,"three")]) !? 1
    > Just "one"

    fromMaybe "?" (Map.empty !? 1)
    > "?"

    fromMaybe "?" (Map.fromList [(1,"one"), (2,"two"), (3,"three")] !? 1)
    > "one"

.. WARNING::
   **DO NOT** Use ``Map.!``. It is partial and throws a runtime error if the key
   doesn't exist.

Check if a map is empty
"""""""""""""""""""""""

::

    Map.null :: Map k v -> Bool
    Map.null m = ...

:haddock_short:`/Data.Map.Strict#null` returns ``True`` if the map ``m`` is
empty and ``False`` otherwise.

::

    Map.null Map.empty
    > True

    Map.null (Map.fromList [(1,"one")])
    > False

The number of entries in a map
""""""""""""""""""""""""""""""

::

    Map.size :: Map k v -> Int
    Map.size m = ...

:haddock_short:`/Data.Map.Strict#size` returns the number of entries in the map
``m``.

::

    Map.size Map.empty
    > 0

    Map.size (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > 3

Find the minimum/maximum
""""""""""""""""""""""""

*Since version 0.5.9*

::

    Map.lookupMin, Map.lookupMax :: Map k v -> Maybe (k, v)
    Map.lookupMin m = ...
    Map.lookupMax m = ...

:haddock_short:`/Data.Map.Strict#lookupMin` and
:haddock_short:`/Data.Map.Strict#lookupMax` respectively return the
minimum or maximum element of the map ``m``, or ``Nothing`` if the map is empty.

::

    Map.lookupMin Map.empty
    > Nothing

    Map.lookupMin (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > Just (1,"one")

    Map.lookupMax (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > Just (3,"three")

.. WARNING::
   **DO NOT** use ``Map.findMin`` or ``Map.findMax``. They are partial and throw
   a runtime error if the map is empty.

Modification
^^^^^^^^^^^^

Adding a new entry to a map
"""""""""""""""""""""""""""

::

    Map.insert :: Ord k => k -> v -> Map k v -> Map k v
    Map.insert key value m = ...

:haddock_short:`/Data.Map.Strict#insert` adds the ``value`` into the map ``m``
with the given ``key``, replacing the existing value if the key already exists.

::

    Map.insert 1 "one" Map.empty
    > Map.fromList [(1,"one")]

    Map.insert 4 "four" (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"one"),(2,"two"),(3,"three"),(4,"four")]

    Map.insert 1 "uno" (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"uno"),(2,"two"),(3,"three")]


Removing an entry from a map
""""""""""""""""""""""""""""

::

    Map.delete :: Ord k => k -> Map k v -> Map k v
    Map.delete key m = ...

:haddock_short:`/Data.Map.Strict#delete` removes the entry with the specified
``key`` from the map ``m``.  If the key doesn't exist it leaves the map
unchanged.

::

    Map.delete 1 Map.empty
    > Map.empty

    Map.delete 1 (Map.fromList [(1,"one"),(2,"two"),(3,"three")])
    > fromList [(2,"two"),(3,"three")]

Filtering map entries
"""""""""""""""""""""

::

    Map.filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v
    Map.filterWithKey predicate m = ...

:haddock_short:`/Data.Map.Strict#filterWithKey` produces a map consisting of all
entries of ``m`` for which the ``predicate`` returns ``True``.

::

    let f key value = key == 2 || value == "one"
    Map.filterWithKey f (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"one"),(2,"two"]


Modifying a map entry
"""""""""""""""""""""

::

    Map.adjust :: Ord k => (v -> v) -> k -> Map k v -> Map k v
    Map.adjust f key m = ...

:haddock_short:`/Data.Map.Strict#abjust` applies the value transformation
function ``f`` to the entry with given ``key``. If no entry for that key exists
then the map is left unchanged.

::

    Map.alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
    Map.alter f key m = ...

Apply the value transformation function ``f`` to the entry with given ``key``,
if no entry for that key exists then the function is passed ``Nothing``. If the
function returns ``Nothing`` then the entry is deleted, if the function returns
``Just v2`` then the value for the ``key`` is updated to ``v2``. In other words,
alter can be used to insert, update, or delete a value.

::

    import Data.Maybe (isJust)
    let addValueIfMissing mv = if isJust mv then mv else (Just 1)
    Map.alter addValueIfMissing "key" (Map.fromList [("key", 0)])
    > fromList [("key",0)]

    let addValueIfMissing mv = if isJust mv then mv else (Just 1)
    Map.alter addValueIfMissing "new_key" (Map.fromList [("key", 0)])
    > fromList [("key",0),("new_key",1)]

The function ``doubleIfPositivie`` below will need to be placed in a Haskell
source file.

::

    doubleIfPositive :: Maybe Int -> Maybe Int
    doubleIfPositive mv = case mv of
      -- Do nothing if the key doesn't exist.
      Nothing -> Nothing

      -- If the key does exist, double the value if it is positive.
      Just v -> if v > 0 then (Just v*2) else (Just v)

    -- In GHCi
    Map.alter doubleIfPositive "a" (Map.fromList [("a", 1), ("b", -1)])
    > Map.fromList [("a",2), ("b",-1)]

    Map.alter doubleIfPositive "b" (Map.fromList [("a", 1), ("b", -1)])
    > Map.fromList [("a", 1), ("b",-1)]

Modifying all map entries (mapping and traversing)
""""""""""""""""""""""""""""""""""""""""""""""""""

::

    Map.map :: (a -> b) -> Map k a -> Map k v
    Map.map f m = ...

    Map.mapWithKey :: (k -> a -> b) -> Map.Map k a -> Map.Map k b
    Map.mapWithKey g m = ...


:haddock_short:`/Data.Map.Strict#map` creates a new map by applying the
transformation function ``f`` to each entries value. This is how `Functor
<https://wiki.haskell.org/Typeclassopedia#Functor>`_ is defined for maps.

:haddock_short:`/Data.Map.Strict#mapWithKey` does the same as ``map`` but gives
you access to the key in the transformation function ``g``.

::

    Map.map (*10) (Map.fromList [("haskell", 45), ("idris", 15)])
    > fromList [("haskell",450),("idris",150)]

    -- Use the Functor instance for Map.
    (*10) <$> Map.fromList [("haskell", 45), ("idris", 15)]
    > fromList [("haskell",450),("idris",150)]

    let g key value = if key == "haskell" then (value * 1000) else value
    Map.mapWithKey g (Map.fromList [("haskell", 45), ("idris", 15)])
    > fromList [("haskell",45000),("idris",15)]


You can also apply a function which performs *actions* (such as printing) to
each entry in the map.

::

    Map.traverseWithKey :: Applicative t => (k -> a -> t b) -> Map.Map k a -> t (Map.Map k b)
    Map.traverseWithKey f m = ...

:haddock_short:`/Data.Map.Strict#traverseWithKey` maps each element of the map
``m`` to an *action* that produces a result of type ``b``. The actions are
performed and the values of the map are replaced with the results from the
function. You can think of this as a ``map`` with affects.

::

    -- | Ask the user how they want to schedule a bunch of tasks
    -- that the boss has assigned certain priorities.
    makeSchedule :: Map Task Priority -> IO (Map Task DateTime)
    makeSchedule = traverseWithKey $ \task priority ->
      do
        putStrLn $ "The boss thinks " ++ show task ++
	             " has priority " ++ show priority ++
                     ". When do you want to do it?"
        readLn



Set-like Operations
^^^^^^^^^^^^^^^^^^^

.. _union:

Union
"""""

::

    Map.unionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
    Map.unionWith f l r = ...

:haddock_short:`/Data.Map.Strict#union` returns a map containing all entries that
are keyed in either of the two maps. If the same key appears in both maps, the
value is determined by calling ``f`` passing in the left and right value (`set
union <https://en.wikipedia.org/wiki/Union_(set_theory)>`_).

::


    Map.unionWith (++) Map.empty (Map.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"x"),(2,"y")]

    let f lv rv = lv
    Map.unionWith f (Map.fromList [(1, "a")]) (Map.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"a"),(2,"y")]

    Map.unionWith (++) (Map.fromList [(1, "a")]) (Map.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"ax"),(2,"y")]


Intersection
""""""""""""

::

    Map.intersectionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
    Map.intersectionWith f l r = ...

:haddock_short:`/Data.Map.Strict#intersection` returns a map containing all
entries that have a key in both maps ``l`` and ``r``. The value in the returned
map is determined by calling ``f`` on the values from the left and right map
(`set intersection <https://en.wikipedia.org/wiki/Intersection_(set_theory)>`_).

::

    Map.intersectionWith (++) Map.empty (Map.fromList [(1,"x"), (2,"y")])
    > fromList []

    Map.intersectionWith (++) (Map.fromList [(1, "a")]) (Map.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"ax")]



Difference
""""""""""

::

    Map.difference :: Ord k => Map k v -> Map k v -> Map k v
    Map.difference l r = ...

:haddock_short:`/Data.Map.Strict#difference` returns a map containing all entries
that have a key in the ``l`` map but not the ``r`` map (`set difference/relative
complement
<https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement>`_).

::

    Map.difference (Map.fromList [(1,"one"), (2,"two"), (3,"three")]) Map.empty
    > fromList [(1,"uno"),(2,"two"),(3,"three")]

    Map.difference (Map.fromList[(1,"one"), (2,"two")]) (Map.fromList [(1,"uno")])
    > fromList [(2,"two")]


Serialization
-------------

The best way to serialize and deserialize maps is to use one of the many
libraries which already support serializing maps. :haddock:`binary`,
:haddock:`cereal`, and :haddock:`store` are some common libraries that people
use.

.. TIP::
   If you are writing custom serialization code use
   :haddock_short:`/Data.Map.Strict#fromDistinctAscList` (see
   `#405 <https://github.com/haskell/containers/issues/405>`_ for more info).


Performance
-----------

The API docs are annotated with the Big-*O* complexities of each of the map
operations. For benchmarks see the `haskell-perf/dictionaries
<https://github.com/haskell-perf/dictionaries>`_ page.


Looking for more?
-----------------

Didn't find what you're looking for? This tutorial only covered the most common
map functions, for a full list of functions see the
:haddock_short:`/Data.Map.Strict#Map` and
:haddock_short:`/Data.IntMap.Strict#IntMap` API documentation.
