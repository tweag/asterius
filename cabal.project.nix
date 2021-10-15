packages:
  asterius
  ghc-toolkit
  wasm-toolkit

source-repository-package
  type:     git
  location: https://github.com/tweag/inline-js.git
  tag:      0fc7444c552820e44ea54ae82eb1f8542dd56f36
  subdir:   inline-js-core

source-repository-package
  type:     git
  location: https://github.com/tweag/ghc-asterius.git
  tag:      7db476e9d7c28e1bf70391b0f276b657acce6bf0
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
