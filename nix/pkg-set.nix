{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays ++ [
        (import "${sources.wasi-sdk}/nix/binaryen.nix")
        (import ./libghcconstants.nix)
      ];
    })
, ghc ? "ghc8105"
}:
pkgs.callPackage
  ({ callPackage, haskell-nix, nodejs_latest }:
    haskell-nix.cabalProject {
      src = haskell-nix.haskellLib.cleanGit {
        name = "asterius_src";
        src = ../.;
      };
      compiler-nix-name = ghc;
      modules = [
        { dontPatchELF = false; }
        { dontStrip = false; }
        { hardeningDisable = [ "all" ]; }
        {
          packages.inline-js-core.preConfigure = ''
            substituteInPlace src/Language/JavaScript/Inline/Core/NodePath.hs --replace '"node"' '"${nodejs_latest}/bin/node"'
          '';
        }
      ];
    })
{ }