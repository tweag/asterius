{ pkgs ? import <nixpkgs> { } }:
pkgs.callPackage
  ({ nodePackages, stdenvNoCC }:
    let
      src = ./.;
      src_configured = stdenvNoCC.mkDerivation {
        name = "asdf-src-configured";
        inherit src;
        nativeBuildInputs = [ nodePackages.node2nix ];
        buildPhase = "node2nix -l package-lock.json -d -14";
        installPhase = ''
          mkdir $out
          cp \
            default.nix \
            node-env.nix \
            node-packages.nix \
            package.json \
            package-lock.json \
            $out
        '';
      };
      node_dependencies =
        (import src_configured { inherit pkgs; }).nodeDependencies;
    in
    node_dependencies)
{ }
