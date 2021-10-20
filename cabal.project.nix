packages:
  asterius
  ghc-toolkit
  wasm-toolkit

source-repository-package
  type:     git
  location: https://github.com/tweag/inline-js.git
  tag:      8512b09d2c0533a41d5d2aef182b11a58c420c10
  subdir:   inline-js-core

source-repository-package
  type:     git
  location: https://github.com/tweag/ghc-asterius.git
  tag:      6a375ebc374ed83ac568725de945bd751756a401
  subdir:
    ahc-bin
    ahc-pkg
    ghc-asterius
    ghc-boot-asterius
    ghc-boot-th-asterius
    ghc-heap-asterius
    ghci-asterius
    template-haskell-asterius

package aeson
  flags: +cffi

package binaryen
  flags: +system-binaryen

package hashable
  flags: +random-initial-seed
