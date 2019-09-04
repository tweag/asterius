# About the custom GHC fork

Asterius is currently based on a custom GHC fork maintained
[here](https://github.com/TerrorJack/ghc). The default branch is `asterius-8.6`,
adding a few custom patches on top of the latest GHC 8.6.x release.

See the [commits](https://github.com/TerrorJack/ghc/commits/asterius-8.6) on top
of the upstream release commit for our modifications:

* Enable Michael Sloan's [D4986](https://phabricator.haskell.org/D4986) and
  [D4904](https://phabricator.haskell.org/D4904) for hacking the custom GHC with
  `ghcid`.
* Enable Joachim Breitner's [D5079](https://phabricator.haskell.org/D5079) and
  [D5082](https://phabricator.haskell.org/D5082) for configurable integer
  library and tables-next-to-code in `DynFlags`.
* Implement additional
  [`Hooks`](https://github.com/TerrorJack/ghc/blob/asterius-8.6/compiler/main/Hooks.hs).
* Various modifications due to changed type signatures in the hooked functions.
* Link `ghc-pkg`/`hsc2hs` with `-threaded`.

See the [`circleci-ghc-8.6`](circleci-ghc-8.6) branch of `asterius` repo for CI
scripts to build ghc bindists for the fork. The bindist artifacts are then used
in `stack.yaml` of `asterius`.
