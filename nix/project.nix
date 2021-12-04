{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays
        ++ [ (import ./binaryen.nix) ];
    })
, ghc ? "ghc8107"
}:
pkgs.callPackage
  ({ haskell-nix, nodejs_latest }:
    haskell-nix.cabalProject rec {
      src = haskell-nix.haskellLib.cleanGit {
        name = "asterius_src";
        src = ../.;
      };
      cabalProject = builtins.readFile "${src}/nix.cabal.project";
      compiler-nix-name = ghc;
      modules = [
        {
          packages.inline-js-core.preConfigure = ''
            substituteInPlace src/Language/JavaScript/Inline/Core/NodePath.hs --replace '"node"' '"${nodejs_latest}/bin/node"'
          '';
        }
      ];
    })
{ }
