{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays ++ [
        (import ./nix/binaryen.nix)
        (import "${sources.wasi-sdk}/nix/wasmtime.nix")
        (import ./nix/libghcconstants.nix)
        (import "${sources.ghc-asterius}/nix/wizer.nix")
      ];
    })
, ghc ? "ghc8107"
, hsPkgs ? pkgs.callPackage ./nix/pkg-set.nix { inherit pkgs ghc; }
}:
(hsPkgs.shellFor rec {
  packages = ps: with ps; [ asterius ghc-toolkit wasm-toolkit ];

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
      pkgs.wasmtime
      (import ./webpack/default.nix { inherit pkgs; })
      pkgs.wizer
    ];

  buildInputs = [ pkgs.libffi ];

  exactDeps = true;

  AHC_RTS =
    let p = import "${sources.ghc-asterius}/nix/wasi-rts.nix" { };
    in "${p}/bin/rts.wasm";

  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.ghcconstants ];

  WASI_SDK_PREFIX = import "${sources.wasi-sdk}/nix/default.nix" { };

  GHC_ASTERIUS = import "${sources.ghc-asterius}/nix/src.nix" { };

  GHC_ASTERIUS_BOOT = GHC_ASTERIUS.boot;

  shellHook = ''
    taskset -pc 0-1000 $$

    pushd $(git rev-parse --show-toplevel)

    for pkg in asterius ghc-toolkit wasm-toolkit; do
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
