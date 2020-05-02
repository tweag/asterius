with import <nixpkgs> {};
haskell.compiler.ghc883.overrideAttrs (old: {
  patches = [
    ./0001-D5082.patch
    ./0002-asterius.patch
    ./0003-asterius-iserv.patch
    ./0004-expose-internals.patch
  ];
})
