# To use this run `nix-shell`.  In the shell run `stack --stack-yaml stack-ghc864.yaml build`
# to build the asterius executables.  You can then boot asterius with
# `stack --stack-yaml stack-ghc864.yaml exec ahc-boot`
let
  nixpkgs = import <nixpkgs> {};
  nix-tools = (import ./. {}).nix-tools;
  hsPkgs = nix-tools._raw.hsPkgs;
in (hsPkgs.shellFor {
    # Shell will provide the dependencies of asterius, but not asterius itself.
    packages = ps: with ps; [
      asterius
      binaryen
      ghc-toolkit
      wabt
      ghc-toolkit
      inline-js
      inline-js-core
      wabt
      wasm-toolkit ];
  }).overrideAttrs (oldAttrs: {
    shellHook = (oldAttrs.shellHook or "") + ''
      unset CABAL_CONFIG
      export boot_libs_path=${nix-tools._raw.ghc864.boot-libs}
    '';
  })