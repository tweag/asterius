{ sources ? import ../../nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays
      ++ [ (import ../../nix/binaryen.nix) ];
  })
, config ? {}
, overlays ? []
}:
pkgs // {
  wasi_sdk = import "${import ../../nix/wasi-sdk.nix { }}/nix/default.nix" { };
  src32 = import "${sources.ghc-asterius}/nix/src-32.nix" { };
  wasmtime = (pkgs.callPackage "${import ../../nix/wasi-sdk.nix { }}/nix/wasmtime.nix" { });
  wizer = (pkgs.callPackage "${sources.ghc-asterius}/nix/wizer.nix" { });
  src = import "${sources.ghc-asterius}/nix/src.nix" { }; 
  ahc_rts = import "${sources.ghc-asterius}/nix/wasi-rts.nix" { };
  wasm = import ../../nix/ghcconstants.nix {};
}
