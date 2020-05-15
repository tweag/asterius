{pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  buildInputs = [ zstd ];
}
