{ sources ? import ./sources.nix { }
, binaryenOverlay ? import ./binaryenOverlay.nix
, haskellNix ? import sources.haskell-nix { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs // { overlays = haskellNix.nixpkgsArgs.overlays ++ [ binaryenOverlay ]; }
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: pkgs
