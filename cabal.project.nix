packages:
  asterius
  ghc-toolkit

source-repository-package
  type:     git
  location: https://github.com/tweag/inline-js.git
  tag:      8512b09d2c0533a41d5d2aef182b11a58c420c10
  subdir:   inline-js-core

source-repository-package
  type:     git
  location: https://github.com/tweag/ghc-asterius.git
  tag:      bf758f1f98aab4f3267261bbeb91a18fa2e8de07
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
