{ pkgs ? import ./bazel_deps.nix {} }:
with pkgs;
mkShell {
  LANG="C.UTF-8";
  buildInputs = [gcc cacert git nix bazel_4 ];
}
