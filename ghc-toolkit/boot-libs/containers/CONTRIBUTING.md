# Instructions for Contributors

To report bugs, please use the [GitHub issue tracker](https://github.com/haskell/containers/issues).
We also appreciate [pull requests](https://github.com/haskell/containers/pulls) on GitHub.

For proposing API changes or enhancements, please follow the [guidelines outlined on the Haskell Wiki](https://wiki.haskell.org/Library_submissions#Guide_to_proposers).
All such changes should be discussed on the libraries@haskell.org mailing list.


## Building, testing, and benchmarking

Building, testing, and benchmarking the containers package can be done using either `cabal-install` or `stack`.

### With cabal-install

Minimum cabal version: 1.24

_Note: The procedure here is a little weird because cabal configure is unable to solve for the constraints. We're looking into why that is ([#462](https://github.com/haskell/containers/issues/462))._

```
cabal sandbox init
cabal install --only-dependencies
# Install test dependencies
cabal install 'test-framework >= 0.3.3' 'test-framework-quickcheck2 >= 0.2.9' 'QuickCheck >= 2.4.0.1' 'ChasingBottoms' 'HUnit' 'test-framework-hunit'
# Install benchmark dependencies
cabal install 'criterion'
# If you only need tests or benchmarks, you can omit the other --enable-xyz flag.
cabal configure -v2 --enable-tests --enable-benchmarks
cabal build
cabal test
cabal bench
``` 


### With [Stack](https://docs.haskellstack.org/en/stable/README/)

Minimum stack version: 1.6.1

```
stack build
stack test
stack bench
```


## Troubleshooting

- If you're using Stack, make sure you have version >= 1.6.1
  ([stack#3524](https://github.com/commercialhaskell/stack/issues/3624),
  [stack#3345](https://github.com/commercialhaskell/stack/issues/3345)).


## Sending Pull Requests

When you send a pull request, please:

- Link to the libraries@haskell.org discussion thread if you are changing the
  public API.

- If you are requesting a change that is likely to affect performance, we will
  be able to evaluate it better if you include the results of running the
  benchmarks before and after. If the current benchmarks cannot demonstrate
  a desired difference, please try to add one or more new benchmarks to do so.
  If there are significant changes, please include the benchmark results in
  your commit message.

- If you are requesting a change that adds new functionality or affects
  behaviour, please add QuickCheck properties exercising the code if they
  do not already exist. If you are fixing a bug that occurs too rarely for
  QuickCheck to hit reliably then consider adding unit tests as well.
  
- Update the change log for non-trivial changes.

- Let us know how you wish to be credited in the changelog.

## Docs

The internal docs are generated using Haddock which can be invoked with `cabal
haddock` or `stack haddock`.

The external docs are served by ReadTheDocs at
https://haskell-containers.readthedocs.io and live in the `docs/` directory. To
build the docs locally run `pip install sphinx sphinx-autobuild` to install the
dependencies, `git submodule update --init`, and then `cd docs/ && make html`.
