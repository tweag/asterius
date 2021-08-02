{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays ++ [
        (import "${sources.wasi-sdk}/nix/binaryen.nix")
        (import "${sources.wasi-sdk}/nix/wasmtime.nix")
        (import ./wizer.nix)
      ];
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
      src = sources.haskell-language-server;
      compiler-nix-name = ghc;
      configureArgs = "--disable-benchmarks --disable-tests";
      modules = [{ dontPatchELF = false; } { dontStrip = false; }];
    }).haskell-language-server.components.exes.haskell-language-server
    pkgs.haskell-nix.internal-cabal-install
    pkgs.haskell-nix.internal-nix-tools
    pkgs.cacert
    pkgs.git
    pkgs.niv
    pkgs.nixfmt
    pkgs.nixpkgs-fmt
    pkgs.nodejs_latest
    pkgs.wabt
    pkgs.wasmtime
    pkgs.wizer
  ];

  exactDeps = true;

  WASI_SDK_PREFIX = import "${sources.wasi-sdk}/nix/default.nix" { };

  shellHook = ''
    pushd $(git rev-parse --show-toplevel)
    for pkg in asterius ghc-toolkit wasm-toolkit; do
      hpack $pkg
    done
    popd
  '';
}
