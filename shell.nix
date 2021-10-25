{ sources ? import ./nix/sources.nix { }
, pkgs ? import sources.nixpkgs {
    overlays = [
      (import ./nix/binaryenOverlay.nix)
    ];
  }
}:
(pkgs.buildFHSUserEnv {
  name = "hs-fhs";
  targetPkgs = ps: (with ps; [
    haskellPackages.alex
    autoconf
    automake
    binaryen
    binutils
    gcc
    haskell.compiler.ghc884
    git
    gmp.dev
    haskellPackages.happy
    ncurses.dev
    nodejs-15_x
    (python39.withPackages (ps: with ps; [
      recommonmark
      sphinx
    ]))
    stack
    zlib.dev
  ]);
  profile = ''
    export LANG=en_US.utf8
    unset NIX_SSL_CERT_FILE
    unset SSL_CERT_FILE
  '';
}).env
