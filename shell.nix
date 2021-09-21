{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays ++ [
        (import ./nix/binaryen.nix)
        (import "${sources.wasi-sdk}/nix/wasmtime.nix")
        (import ./nix/libghcconstants.nix)
        (import ./nix/wizer.nix)
      ];
    })
, ghc ? "ghc8107"
, hsPkgs ? pkgs.callPackage ./nix/pkg-set.nix { inherit pkgs ghc; }
}:
(hsPkgs.shellFor rec {
  packages = ps: with ps; [ asterius ghc-toolkit wasm-toolkit ];

  withHoogle = true;

  nativeBuildInputs = pkgs.lib.attrValues
    (removeAttrs (import sources.hs-nix-tools { inherit ghc; }) [ "cabal" ])
  ++ [
    hsPkgs.ahc-pkg.components.exes.ahc-pkg
    (pkgs.writeShellScriptBin "cabal" ''
      exec ${pkgs.haskell-nix.internal-cabal-install}/bin/cabal --project-file=dummy.project "$@"
    '')
    pkgs.binaryen
    pkgs.cacert
    pkgs.git
    pkgs.nodejs_latest
    pkgs.wabt
    pkgs.wasmtime
    pkgs.wizer
  ];

  exactDeps = true;

  AHC_HOST_CABAL = "${pkgs.haskell-nix.internal-cabal-install}/bin/cabal";

  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.ghcconstants ];

  WASI_SDK_PREFIX = import "${sources.wasi-sdk}/nix/default.nix" { };

  GHC_ASTERIUS = import "${sources.ghc-asterius}/nix/src.nix" {};

  GHC_ASTERIUS_BOOT = GHC_ASTERIUS.boot;

  shellHook = ''
    pushd $(git rev-parse --show-toplevel)

    for pkg in asterius ghc-toolkit wasm-toolkit; do
      hpack $pkg
    done

    export AHC_BOOT_SRCDIR=$PWD/ghc-toolkit/boot-libs
    export AHC_LIBDIR=$PWD/sysroot
    export PATH=$PATH:$PWD/dist-newstyle/build/x86_64-linux/ghc-8.10.7/asterius-0.0.1/x/ahc/build/ahc:$PWD/dist-newstyle/build/x86_64-linux/ghc-8.10.7/asterius-0.0.1/x/ahc-cabal/build/ahc-cabal:$PWD/dist-newstyle/build/x86_64-linux/ghc-8.10.7/asterius-0.0.1/x/Setup-ghc-prim/build/Setup-ghc-prim

    mkdir -p ~/.ahc-cabal
    cp asterius/cabal/config ~/.ahc-cabal

    popd
  '';
}).overrideAttrs (_: { installPhase = "export > $out"; })
