let
  localLib = import ./nix/lib.nix;
  disabled = [
  ];
in
{ asterius ? { outPath = ./.; rev = "abcdef"; } ,... }@args:
localLib.pkgs.lib.mapAttrsRecursiveCond
(as: !(as ? "type" && as.type == "derivation"))
(path: v: if (builtins.elem path disabled) then null else v)
(localLib.nix-tools.release-nix {
  _this = asterius;
  package-set-path = ./.;

  packages = [ "asterius" ];

  required-name = "asterius-required-checks";
  required-targets = jobs: [
#    jobs.nix-tools.libs.asterius.x86_64-darwin
    jobs.nix-tools.libs.asterius.x86_64-linux
  ];

} (builtins.removeAttrs args ["asterius"]))
