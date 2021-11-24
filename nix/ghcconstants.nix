{ sources ? import ./sources.nix { }, pkgs ? import sources.nixpkgs { } }:
let
  ghc_asterius = import "${sources.ghc-asterius}/nix/src-32.nix" { };
  wasi-sdk = import "${sources.wasi-sdk}/nix/default.nix" { };
in
pkgs.callPackage
  ({ stdenv, stdenvNoCC }:
    stdenvNoCC.mkDerivation ({
      name = "ghcconstants";
      src = ../ghc-toolkit/cbits;
      installPhase = ''
        mkdir -p $out/bin
        $CC \
          -Wall \
          -Wextra \
          -I${ghc_asterius}/ghc-asterius/autogen \
          -mexec-model=reactor \
          -o $out/bin/ghcconstants.wasm \
          ghc_constants.c
      '';
      allowedReferences = [ ];
    } // (import "${sources.ghc-asterius}/nix/wasi-sdk-stdenv.nix" {
      inherit stdenv wasi-sdk;
    })))
{ }
