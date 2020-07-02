let
  rev = "91fce0fb2f4b79c6e4a3f4ea129c4e665197cd35";
  sha256 = "14adfhma5xnzs3f1n9nqpr4x9yvw9b100q2rprwyvy612c82pvz0";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
