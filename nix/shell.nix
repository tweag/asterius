{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs
    (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays ++ [
        (import "${sources.wasi-sdk}/nix/binaryen.nix")
        (import "${sources.wasi-sdk}/nix/wasmtime.nix")
        (import ./libghcconstants.nix)
        (import ./wizer.nix)
      ];
    })
, ghc ? "ghc8105"
, hsPkgs ? pkgs.callPackage ./pkg-set.nix { inherit pkgs ghc; }
}:
(hsPkgs.shellFor {
  packages = ps: with ps; [ asterius ghc-toolkit wasm-toolkit ];

  withHoogle = false;

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
    pkgs.niv
    pkgs.nixfmt
    pkgs.nixpkgs-fmt
    pkgs.nodejs_latest
    pkgs.wabt
    pkgs.wasmtime
    pkgs.wizer
  ];

  exactDeps = true;

  AHC_HOST_CABAL = "${pkgs.haskell-nix.internal-cabal-install}/bin/cabal";

  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.ghcconstants ];

  WASI_SDK_PREFIX = import "${sources.wasi-sdk}/nix/default.nix" { };

  shellHook = ''
    pushd $(git rev-parse --show-toplevel)
    for pkg in asterius ghc-toolkit wasm-toolkit; do
      hpack $pkg
    done
    popd
  '';
}).overrideAttrs (_: { installPhase = "printenv | sort > $out"; })
