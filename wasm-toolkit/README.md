# `wasm-toolkit`

A Haskell library to handle WebAssembly code. Currently contains:

* Complete definition of the AST, including support for custom sections.
* Binary encoder/decoder.
* Stochastic fuzzer which generates syntactically correct AST to test binary encoder/decoder.

This library models WebAssembly in a "speccy" manner; it only deals with lists of instructions, and does not implement expression syntax trees. The library doesn't have third party dependencies other than boot libs (the test suite does rely on `QuickCheck`)
