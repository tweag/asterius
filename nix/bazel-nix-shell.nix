({ sources ? import ./sources.nix { }
, pkgs ? import sources.nixpkgs {
    overlays = [
      (import ./binaryenOverlay.nix)
    ];
  }
}:
with pkgs;
pkgs.mkShell {
    nativeBuildInputs = [
    haskellPackages.alex
    bash
    nix
    cabal-install
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
    ];

    shellHook = ''
  #export SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"
  #export NIX_SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"

  # One of the two must be enabled for git clone to work (in the utils/make-packages.py)
  export GIT_SSL_CAINFO="/etc/ssl/certs/ca-certificates.crt"
  #export GIT_SSL_NO_VERIFY=true


  #export NIX_PATH="nixpkgs=/home/stan/test/bazel/workspace_rules/bazel-workspace_rules/external/rules_haskell/nixpkgs"
  #export NIX_PATH="nixpkgs=/home/stan/src/asterius/nix"
'';
}){}
