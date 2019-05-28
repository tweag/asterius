let
  # Use this to set the version of asterius to be booted
  cached = import ((import <nixpkgs> {}).fetchgit {
    url = "https://github.com/input-output-hk/asterius";
    rev = "b939a8e63d71a63450605bb8d26ce247e7e16157";
    sha256 = "12ykihra6siy87snvax4pq5m4d4d5iyjxyd6rn0fpi9j4lbglnrj";
    fetchSubmodules = true;
  }) {};
  nix-tools = (import ./. {}).nix-tools;
  hsPkgs = nix-tools._raw.hsPkgs;
in (hsPkgs.shellFor {
    # Shell will provide the dependencies of asterius, but not asterius itself.
    packages = ps: with ps; [ asterius binaryen ];
  }).overrideAttrs (oldAttrs: {
    shellHook = (oldAttrs.shellHook or "") + ''
      unset CABAL_CONFIG
      export asterius_bootdir=${cached.nix-tools._raw.asterius-boot}/boot
      find . -name package.yaml -exec hpack "{}" \;
    '';
  })
