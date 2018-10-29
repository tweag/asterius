# About the custom GHC fork

Asterius currently is based on a custom GHC fork maintained [here](https://github.com/TerrorJack/ghc/tree/asterius). We regularly merge `master` commits back, build new bindists and use them on CI, to ensure our fork doesn't get bit-rotten and become painful to upstream back.

Here is a *complete* list of differences we've made in the fork (surprisingly few at the moment):

* Enable [D5079](https://phabricator.haskell.org/D5079) and [D5082](https://phabricator.haskell.org/D5082), which are kindly offered by Joachim Breitner but not all landed in `master` yet.
* Implement additional [`Hooks`](https://github.com/TerrorJack/ghc/blob/asterius/compiler/main/Hooks.hs): `tcRnModuleHook`, `stgCmmHook`, `cmmToRawCmmHook`.

See the `circle-ghc-bindist`/`appveyor-ghc-bindist` branches of `asterius` repo for CI scripts to build bindists for the fork.
