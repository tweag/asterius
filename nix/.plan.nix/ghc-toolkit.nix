{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "ghc-toolkit"; version = "0.0.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2018 Tweag I/O";
      maintainer = "Shao Cheng <cheng.shao@tweag.io>";
      author = "";
      homepage = "https://github.com/tweag/asterius#readme";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base))
        (hsPkgs.buildPackages.containers or (pkgs.buildPackages.containers))
        (hsPkgs.buildPackages.directory or (pkgs.buildPackages.directory))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.Cabal)
          (hsPkgs.base)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.ghc)
          (hsPkgs.process)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../ghc-toolkit; }) // {
    cabal-generator = "hpack";
    }