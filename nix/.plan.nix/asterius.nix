{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "asterius"; version = "0.0.1"; };
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
        hsPkgs.buildPackages.ghc-toolkit
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.Cabal)
          (hsPkgs.base)
          (hsPkgs.binary)
          (hsPkgs.binaryen)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.deepseq)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.ghc)
          (hsPkgs.ghc-prim)
          (hsPkgs.ghc-toolkit)
          (hsPkgs.inline-js)
          (hsPkgs.mtl)
          (hsPkgs.parsec)
          (hsPkgs.process)
          (hsPkgs.transformers)
          (hsPkgs.wasm-toolkit)
          ];
        };
      exes = {
        "ahc" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "ahc-boot" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "ahc-cabal" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "ahc-dist" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "ahc-ld" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "ahc-link" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "ahc-pkg" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        };
      tests = {
        "array" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "bigint" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "bytearray" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "cloudflare" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "fib" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "jsffi" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "rtsapi" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "stableptr" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "teletype" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        "todomvc" = {
          depends = [
            (hsPkgs.Cabal)
            (hsPkgs.asterius)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.binaryen)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.ghc)
            (hsPkgs.ghc-prim)
            (hsPkgs.ghc-toolkit)
            (hsPkgs.inline-js)
            (hsPkgs.mtl)
            (hsPkgs.parsec)
            (hsPkgs.process)
            (hsPkgs.transformers)
            (hsPkgs.wasm-toolkit)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../asterius; }) // {
    cabal-generator = "hpack";
    }
