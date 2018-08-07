``containers`` Introduction
===========================

The ``containers`` package provides implementations of various immutable data
structures.

Some of the data structures provided by this package have a very large API
surface (for better or worse). The docs here focus on the most common functions
which should be more than enough to get you started. Once you know the basics,
or if you're looking for a specific function, you can head over to the
:haddock:`containers` Haddocks to check out the full API documentation!

Provided Data Structures
------------------------

- :doc:`set`: ordered, non-duplicated elements
- :doc:`map`: ordered maps from keys to values (aka. dictionaries)
- :doc:`sequence`: finite sequence of elements, an efficient alternative to list

.. NOTE::
   You'll need ``containers >= 0.5.9`` for a few of the examples. See
   `Version Requirements`_ for info on how to check which version you have and
   how to upgrade.


Related Packages
----------------

- :haddock:`unordered-containers` - containers using hashing instead of
  ordering.

- :haddock:`array` - mutable and immutable arrays.

- :haddock:`vector` - efficient ``Int``-indexed arrays (boxed and unboxed).

- :haddock:`bytestring` - compact, immutable bytestrings, useful for binary and
  8-bit character data.

- :haddock:`dlist` - difference lists with *O(1)* append, useful for efficient
  logging and pretty printing.

- :haddock:`hashtables` - mutable hash tables in the ST monad.


Looking for more resources?
---------------------------

If you've worked your way through the documentation here and you're looking for
more examples or tutorials you should check out:

- `haskell-lang.org's containers tutorial
  <https://haskell-lang.org/library/containers>`_
- `Learn You a Haskell "Modules" chapter <http://learnyouahaskell.com/modules>`_

.. _installing:

Installing and using the ``containers`` package
-----------------------------------------------

Version Requirements
^^^^^^^^^^^^^^^^^^^^

For some of the examples you'll need ``containers >= 0.5.9`` which ships with
``GHC >= 8.2``. You can check to see which version you have installed with:

::

    ghc --version
    > The Glorious Glasgow Haskell Compilation System, version 8.2.2

If you have an older version, don't worry about it, the majority of the code
works with older versions of the package. If you want, you can get a recent
version by `from haskell.org <https://www.haskell.org/downloads>`_, or with
`Stack <https://www.haskellstack.org>`_ using ``stack --resolver lts-10.2
ghci``.


Importing modules
^^^^^^^^^^^^^^^^^

All of the modules in ``containers`` should be imported ``qualified`` since they
use names that conflict with the standard Prelude.

::

    import qualified Data.Set as Set
    import qualified Data.Map.Strict as Map
    import qualified Data.Sequence as Seq


In GHCi
^^^^^^^

Start the GHCi `REPL
<https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop>`_ with
``ghci`` or ``stack ghci``. Once the REPL is loaded import the modules you want
to use and you're good to go!


In a `Cabal <https://cabal.readthedocs.io>`_ or `Stack <https://www.haskellstack.org>`_ project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Add ``containers`` to the ``build-depends:`` stanza for your library,
executable, or test-suite::

    library
        build-depends:
	    base >= 4.3 && < 5,
	    containers >= 0.5.7 && < 0.6

and ``import`` any modules you need in your Haskell source files.
