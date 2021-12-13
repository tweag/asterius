{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays
      ++ [ (import ./nix/binaryen.nix) ];
  })
, ghc ? "ghc8107"
, hsPkgs ? import ./nix/project.nix { inherit pkgs ghc; }
}:
(hsPkgs.shellFor rec {
  packages = ps: with ps; [ asterius ];

  withHoogle = true;

  nativeBuildInputs =
    pkgs.lib.attrValues (import sources.hs-nix-tools { inherit ghc; }) ++ [
      hsPkgs.ahc-pkg.components.exes.ahc-pkg
      pkgs.binaryen
      pkgs.cacert
      pkgs.git
      pkgs.nodejs_latest
      pkgs.util-linux
      pkgs.wabt
      (pkgs.callPackage "${import ./nix/wasi-sdk.nix { }}/nix/wasmtime.nix" { })
      (import ./webpack/default.nix { inherit pkgs; })
      (pkgs.callPackage "${sources.ghc-asterius}/nix/wizer.nix" { })
    ];

  buildInputs = [ pkgs.libffi ];

  exactDeps = true;

  AHC_RTS =
    let p = import "${sources.ghc-asterius}/nix/wasi-rts.nix" { };
    in "${p}/bin/rts.wasm";

  WASI_SDK_PREFIX =
    import "${import ./nix/wasi-sdk.nix { }}/nix/default.nix" { };

  GHC_ASTERIUS = import "${sources.ghc-asterius}/nix/src-32.nix" { };

  GHC_ASTERIUS_BOOT = GHC_ASTERIUS.boot;

  AHC_CONSTANTS =
    let p = import ./nix/ghcconstants.nix { };
    in "${p}/bin/ghcconstants.wasm";

  shellHook = ''
    taskset -pc 0-1000 $$

    pushd $(git rev-parse --show-toplevel)

    for pkg in asterius; do
      hpack $pkg
    done

    export AHC_SRCDIR=$PWD
    export AHC_LIBDIR=$PWD/sysroot
    export PATH=$PATH:$PWD/dist-newstyle/build/x86_64-linux/ghc-8.10.7/asterius-0.0.1/x/ahc/build/ahc:$PWD/dist-newstyle/build/x86_64-linux/ghc-8.10.7/asterius-0.0.1/x/ahc-cabal/build/ahc-cabal:$PWD/dist-newstyle/build/x86_64-linux/ghc-8.10.7/asterius-0.0.1/x/ahc-ld/build/ahc-ld:$PWD/dist-newstyle/build/x86_64-linux/ghc-8.10.7/asterius-0.0.1/x/ahc-link/build/ahc-link:$PWD/dist-newstyle/build/x86_64-linux/ghc-8.10.7/asterius-0.0.1/x/Setup-ghc-prim/build/Setup-ghc-prim

    mkdir -p ~/.ahc-cabal
    cp asterius/cabal/config ~/.ahc-cabal

    popd
  '';
}).overrideAttrs (_: { installPhase = "export > $out"; })
