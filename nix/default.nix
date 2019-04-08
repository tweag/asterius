{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, ...
}:
let
  # our packages
  plan-pkgs = import ./pkgs.nix;

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (plan-pkgs.extras {}).compiler or
             (plan-pkgs.pkgs {}).compiler;

  pkgSet = haskell.mkCabalProjectPkgSet {
    inherit plan-pkgs;
    # The extras allow extension or restriction of the set of
    # packages we are interested in. By using the stack-pkgs.extras
    # we restrict our package set to the ones provided in stack.yaml.
    pkg-def-extras = [
      iohk-extras.${compiler.nix-name}
    ];
    modules = [
      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module
      ({ config, ...}: {
          # packages.hsc2hs.components.exes.hsc2hs.doExactConfig = true;
          packages.ghc.patches = [ ./patches/ghc.patch ];
      })
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
