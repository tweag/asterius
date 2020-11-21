{ pkgs ? import ./nix/nixpkgs.nix { }
, ghc ? pkgs.haskell.compiler.ghc884
}:

with pkgs;

haskell.lib.buildStackProject {
  name = "asterius";
  inherit ghc;
  buildInputs = [
    autoconf
    binaryen
    cabal-install
    cacert
    curl
    nodejs-14_x
  ];
}
