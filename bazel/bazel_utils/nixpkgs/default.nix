let
  # 2021-02-19
  sha256 = "1fwl898f6wznkjpwq11brgadz6iff5w5f4lwj2l7ax2rz7r03mnn";
  rev = "ad4db3f4d8ae54482c63c31c14921cb73953548d";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
