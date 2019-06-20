#
# The defaul.nix file. This will generate targets for all
# buildables.  These include anything from stack.yaml
# (via nix-tools:stack-to-nix) or cabal.project (via
# nix-tools:plan-to-nix). As well as custom definitions
# on top.
#
# nix-tools stack-to-nix or plan-to-nix will generate
# the `nix/plan.nix` file. Where further customizations
# outside of the ones in stack.yaml/cabal.project can
# be specified as needed for nix/ci.
#

# We will need to import the local lib, which will
# give us the iohk-nix tooling, which also includes
# the nix-tools tooling.
{ config ? {}
, system ? builtins.currentSystem
, crossSystem ? null
, ... }@args:
let
  localLib = import ./nix/lib.nix { inherit config system crossSystem; };
in
# This file needs to export a function that takes
# the arguments it is passed and forwards them to
# the default-nix template from iohk-nix. This is
# important so that the release.nix file can properly
# parameterize this file when targetting different
# hosts.
# We will instantiate the defaul-nix template with the
# nix/pkgs.nix file...
localLib.nix-tools.default-nix ./nix/default.nix args
