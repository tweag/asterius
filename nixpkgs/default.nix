let
  # 2020-04-03
  rev = "4dc8447c55fe5262bad1019121a8e6d3f9e1e71f";
  sha256 = "113961iip25h3visfpszrnvwwclkvkgj4x9c7inlh90vcfrk325p";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
