{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays ++ [ (import ./binaryen.nix) ];
    })
, ghc ? "ghc8105"
, toolsGhc ? "ghc8105"
, hsPkgs ? pkgs.callPackage ./pkg-set.nix { inherit pkgs ghc; }
}:
hsPkgs.shellFor {
  packages = ps: with ps; [ asterius ghc-toolkit wasm-toolkit ];

  withHoogle = true;

  tools =
    let
      args = {
        version = "latest";
        compiler-nix-name = toolsGhc;
        modules = [{ dontPatchELF = false; } { dontStrip = false; }];
      };
    in
    {
      brittany = args;
      cabal-fmt = args;
      floskell = args;
      ghcid = args;
      hlint = args;
      hoogle = args;
      ormolu = args;
      stylish-haskell = args;
    };

  nativeBuildInputs = [
    (pkgs.haskell-nix.cabalProject rec {
      src = pkgs.fetchFromGitHub {
        owner = "haskell";
        repo = "haskell-language-server";
        rev = "ghcide-v1.4.0.2";
        sha256 = "sha256-mzIPZS0Ov+xUhb9i1GeACJm7gUZC9D/avle4pJreLdo=";
        fetchSubmodules = true;
      };
      compiler-nix-name = ghc;
      configureArgs = "--disable-benchmarks --disable-tests";
      modules = [{ dontPatchELF = false; } { dontStrip = false; }];
    }).haskell-language-server.components.exes.haskell-language-server
    pkgs.haskell-nix.internal-cabal-install
    pkgs.haskell-nix.internal-nix-tools
    pkgs.niv
    pkgs.nixfmt
    pkgs.nixpkgs-fmt
    pkgs.nodejs_latest
  ];

  exactDeps = true;
}
