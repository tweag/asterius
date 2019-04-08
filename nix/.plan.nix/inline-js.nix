{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "inline-js"; version = "0.0.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2018 Tweag I/O";
      maintainer = "Shao Cheng <cheng.shao@tweag.io>";
      author = "";
      homepage = "https://github.com/tweag/inline-js#readme";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.ghc-prim)
          (hsPkgs.process)
          (hsPkgs.stm)
          (hsPkgs.text)
          ];
        };
      tests = {
        "eval" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc-prim)
            (hsPkgs.inline-js)
            (hsPkgs.process)
            (hsPkgs.stm)
            (hsPkgs.text)
            ];
          };
        "pingpong" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc-prim)
            (hsPkgs.inline-js)
            (hsPkgs.process)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.stm)
            (hsPkgs.text)
            ];
          };
        "puppeteer" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc-prim)
            (hsPkgs.inline-js)
            (hsPkgs.process)
            (hsPkgs.stm)
            (hsPkgs.text)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../inline-js/inline-js; }) // {
    cabal-generator = "hpack";
    }