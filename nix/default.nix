{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, planOnly ? false
, shellOnly ? false
, ...
}:
let
  cleanSrc =
    pkgs.lib.cleanSourceWith {
      src = ./..;
      filter = path: type:
        pkgs.lib.all (i: toString i != path) [ ../.DS_Store ../default.nix ../result ../nix ../asterius-cabal-bin ../asterius-cabal-boot
          # These are .gitignored sow we should exclude them here
          ../.stack-work
          ../wabt/wabt/bin
          ../dist
          ../dist-newstyle
          ../site
          ../.idea
          ../node_modules
          ]
          && pkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".DS_Store" "result"
            # These are .gitignored sow we should exclude them here
            "asterius.cabal"
            "ghc-toolkit.cabal"
            "binaryen.cabal"
            "npm-utils.cabal"
            "wabt.cabal"
            "wasm-toolkit.cabal"
            "yarn.lock"
            "package-lock.json"
          ]
          && !(pkgs.lib.strings.hasInfix ".dump-" (baseNameOf path)) # These are .gitignored sow we should exclude them here
          && pkgs.lib.all (i: !(pkgs.lib.hasSuffix i path)) [ ".lkshf" ".nix" ]
          && pkgs.lib.all (i: !(pkgs.lib.hasPrefix i (baseNameOf path))) [ "result-" ".ghc.environment." ]
          && !(pkgs.lib.hasPrefix (toString ../asterius/test) path && (
                   pkgs.lib.any (i: pkgs.lib.hasSuffix i path) [
                    ".mjs"
                    ".o"
                    ".wasm"
                    ".hi"
                    ".html"
                    ".js" ]
                || pkgs.lib.any (i: i == baseNameOf path) [
                    "package.json"
                    "jsffi_stub.h" ])
              # files that look like build artifacts, but are not
              && !pkgs.lib.any (i: toString i == path) [
                ../asterius/test/cloudflare/cloudflare.mjs
                ../asterius/test/jsffi/jsffi.mjs
                ../asterius/test/rtsapi/rtsapi.mjs
              ]);
    };
  stack = haskell.importAndFilterProject (haskell.callStackToNix {
      src = cleanSrc;
    });
  plan-0 = haskell.importAndFilterProject (haskell.callCabalProjectToNix {
      src = cleanSrc;
      ghc = pkgs.haskell.compiler.ghc865;
      index-state = "2019-06-22T00:00:00Z";
      index-sha256 = "19v6bqgg886704b8palrzqiydnfjsqqkrx9k6c22kw6kffrrrmd6";
    });
  # Hide libiserv from the plan because it does not exist in hackage
  # TODO find a proper fix for this issue
  plan = plan-0 // {
      pkgs = plan-0.pkgs // {
        pkgs = hackage: let x = (plan-0.pkgs.pkgs hackage);
        in x // { packages = removeAttrs x.packages [ "libiserv" ]; }; }; };

  cabalPatch = pkgs.fetchpatch {
    url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
    sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
    stripLen = 1;
  };

  # Node
  nodejs = pkgs.nodejs-11_x;
  nodePkgs = import ./node { inherit pkgs nodejs; };

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  # compiler = (plan.pkgs.extras {}).compiler or
  #            (plan.pkgs.pkgs {}).compiler;
  compilerName = "ghc865";
  # project = stack;
  # mkProjectPkgSet = args: haskell.mkStackPkgSet (args // { stack-pkgs = stack.pkgs; });
  project = plan;
  mkProjectPkgSet = args: haskell.mkCabalProjectPkgSet (args // { plan-pkgs = plan.pkgs; });

  pkgSet = mkProjectPkgSet {
    # The extras allow extension or restriction of the set of
    # packages we are interested in. By using the stack-pkgs.extras
    # we restrict our package set to the ones provided in stack.yaml.
    pkg-def-extras = [
      (hackage: { libiserv = {}; })
      iohk-extras.${compilerName}
    ];
    modules = [
      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module
      { reinstallableLibGhc = true; }
      ({ config, ...}: {
        packages = {
          # packages.hsc2hs.components.exes.hsc2hs.doExactConfig = true;
          ghc.patches = [ ./patches/ghc.patch ./patches/ghc/MR948--32bit-cross-th.patch ];
          wasm-toolkit.package.cleanHpack = true;
          ghc-toolkit.package.cleanHpack = true;
          ghc-toolkit.components.library.extraSrcFiles = [
            "genapply/**/**"
            "boot-libs/**/**"
            "ghc-libdir/**/**"
            ];
          wabt.package.cleanHpack = true;
          wabt.components.library.extraSrcFiles = [
            "wabt/**/**"
            ];
          binaryen.package.cleanHpack = true;
          binaryen.components.library.extraSrcFiles = [
            "binaryen/**/**"
            ];
          asterius.package.cleanHpack = true;
          asterius.package.dataFiles = [
            "rts/*.mjs"
            "boot-init.sh"
            "boot.sh"
            ];
          asterius.components.tests.array.extraSrcFiles = [
            "test/array/**/*.hs"
            ];
          asterius.components.tests.fib.extraSrcFiles = [
            "test/fib/**/*.hs"
            ];
          asterius.components.tests.jsffi.extraSrcFiles = [
            "test/jsffi/**/*.hs"
            ];
          asterius.components.tests.rtsapi.extraSrcFiles = [
            "test/rtsapi/**/*.hs"
            ];
          asterius.components.tests.stableptr.extraSrcFiles = [
            "test/stableptr/**/*.hs"
            ];
          asterius.components.tests.todomvc.extraSrcFiles = [
            "test/todomvc/**/*.hs"
            "test/todomvc/**/*.html"
            ];
          asterius.components.tests.teletype.extraSrcFiles = [
            "test/teletype/**/*.hs"
            ];
          asterius.components.tests.bytearray.extraSrcFiles = [
            "test/bytearray/**/*.hs"
            ];
          asterius.components.tests.bytearraymini.extraSrcFiles = [
            "test/bytearraymini/**/*.hs"
            ];
          asterius.components.tests.bigint.extraSrcFiles = [
            "test/bigint/**/*.hs"
            ];
          asterius.components.tests.cloudflare.extraSrcFiles = [
            "test/cloudflare/**/*.hs"
            ];
          asterius.components.tests.nomain.extraSrcFiles = [
            "test/nomain/**/*.hs"
            ];
          asterius.components.tests.ghc-testsuite.extraSrcFiles = [
            "test/ghc-testsuite/**/*.hs"
            "test/ghc-testsuite/**/*.stdout"
            ];
          asterius.components.tests.exception.extraSrcFiles = [
            "test/exception/**/*.hs"
            ];
          asterius.components.tests.regression60.extraSrcFiles = [
            "test/regression60/**/*.hs"
            ];
          asterius.components.tests.sizeof_md5context.extraSrcFiles = [
            "test/sizeof_md5context/**/*.hs"
            ];
          asterius.components.tests.largenum.extraSrcFiles = [
            "test/largenum/**/*.hs"
            ];
        };
      })
      ({ config, ...}: {
        packages = {
          asterius.components.tests =
            pkgs.lib.mapAttrs (n: v: {
               build-tools =
                 pkgs.lib.optional (!shellOnly) asterius-boot ++ [
                 nodejs
                 nodePkgs.parcel-bundler
                 nodePkgs.todomvc-app-css
                 nodePkgs.todomvc-common ];
             }) (mkProjectPkgSet {})
                 .config.hsPkgs.asterius.components.tests;
        };
      })
    ];
  };
  # Patch file that can be applied to the full ghc tree
  # full-ghc-patch = pkgs.copyPathToStore ./patches/ghc/asterius.patch;
  ghc-head = let
    # Only gitlab has the right submoudle refs (the ones in github mirror do not work)
    # and only fetchgit seems to get the submoudles from gitlab
    ghc-src = pkgs.srcOnly pkgs.haskell.compiler.ghc865;
    ghc-prim = pkgs.fetchzip {
      url = "https://hackage.haskell.org/package/ghc-prim-0.5.3/ghc-prim-0.5.3.tar.gz";
      sha256 = "1inn9dr481bwddai9i2bbk50i8clzkn4452wgq4g97pcgdy1k8mn";
    };
    # The patched libs are currently in the repo
    boot-libs = pkgs.copyPathToStore ../ghc-toolkit/boot-libs;
    # Derive the patch using diff
    patch = pkgs.runCommand "asterius-libs-patch" {
      preferLocalBuild = true;
    } ''
      tmp=$(mktemp -d)
      cd $tmp
      mkdir -p old
      mkdir -p new
      cp -r ${ghc-src}/libraries old/libraries
      ln -s ${boot-libs} new/libraries
      chmod +w -R old
      rm \
        old/libraries/*/configure \
        old/libraries/*/GNUmakefile \
        old/libraries/*/ghc.mk \
        old/libraries/*/Hs*Config.h.in \
        old/libraries/*/*/Hs*Config.h.in \
        old/libraries/*/*/*/Hs*Config.h.in \
        old/libraries/ghc-prim/primops.txt.pp
      cp ${ghc-prim}/GHC/PrimopWrappers.hs old/libraries/ghc-prim/GHC/PrimopWrappers.hs
      mkdir -p old/libraries/rts/sm
      cd new/libraries
      find rts -type f -not -name rts.conf -exec cp ${ghc-src}/"{}" $tmp/old/libraries/"{}" \;
      cd $tmp
      for new in new/libraries/*; do
        (diff -ruN -x '*.rej' -x '*.orig' old/libraries/$(basename $new) $new || true) >> $out
      done
    '';
  in { inherit ghc-src boot-libs patch; };
  ghc865 = let
    ghc-src = pkgs.srcOnly pkgs.haskell.compiler.ghc865;
    ghc-prim = pkgs.fetchzip {
      url = "https://hackage.haskell.org/package/ghc-prim-0.5.3/ghc-prim-0.5.3.tar.gz";
      sha256 = "1inn9dr481bwddai9i2bbk50i8clzkn4452wgq4g97pcgdy1k8mn";
    };
    patch = pkgs.copyPathToStore ./patches/ghc/ghc865-libs.patch;
    ghc-patched-src = pkgs.runCommand "asterius-ghc865-ghc-patched-src" {
      buildInputs = [];
      preferLocalBuild = true;
    } ''
      set +x
      cp -r ${ghc-src} $out
      chmod +w -R $out
      cd $out
      cp -r rts libraries
    '';
    boot-libs = pkgs.runCommand "asterius-ghc865-boot-libs" {
      buildInputs = [ pkgs.haskell.compiler.${compilerName} ];
      preferLocalBuild = true;
    } ''
      set +x
      cp -r ${ghc-patched-src} $out
      chmod +w -R $out
      cd $out/libraries
      patch -p2 < ${patch} || true
      # TODO find a better way to get these
      cp ${ghc-prim}/GHC/Prim.hs ghc-prim/GHC/Prim.hs
      cp ${ghc-prim}/GHC/PrimopWrappers.hs ghc-prim/GHC/PrimopWrappers.hs
      # TODO figure out a better way remove the unwanted stuff from ghc-prim.cabal
      sed -i '96,$ d' ghc-prim/ghc-prim.cabal
      cd $out/libraries/rts
      runghc --ghc-arg=-I$(ghc --print-libdir)/include $out/utils/genapply/Main.hs > AutoApply.cmm
  '';
  in { inherit ghc-src ghc-prim ghc-patched-src boot-libs; };

  asterius-boot = pkgs.runCommand "asterius-boot" {
      preferLocalBuild = true;
      nativeBuildInputs = [ pkgs.makeWrapper pkgs.haskell.compiler.${compilerName} pkgs.autoconf pkgs.automake ];
    } ''
      mkdir -p $out/bin
      mkdir -p $out/boot
      mkdir -p $out/ghc-libdir
      cp -r $(ghc --print-libdir)/include $out/ghc-libdir
      cp $(ghc --print-libdir)/llvm-passes $out/ghc-libdir
      cp $(ghc --print-libdir)/llvm-targets $out/ghc-libdir
      cp $(ghc --print-libdir)/platformConstants $out/ghc-libdir
      cp $(ghc --print-libdir)/template-hsc.h $out/ghc-libdir
      cp -r $(ghc --print-libdir)/settings $out/ghc-libdir
      chmod +w -R $out/ghc-libdir
      cp -r ${../ghc-toolkit/ghc-libdir}/include/* $out/ghc-libdir/include
      ${pkgs.lib.concatMapStringsSep "\n" (exe: ''
        makeWrapper ${pkgSet.config.hsPkgs.asterius.components.exes.${exe}}/bin/${exe} $out/bin/${exe} \
          --prefix PATH : ${nodePkgs.parcel-bundler}/bin \
          --set asterius_bindir $out/bin \
          --set asterius_bootdir $out/boot \
          --set boot_libs_path ${ghc865.boot-libs} \
          --set sandbox_ghc_lib_dir $out/ghc-libdir
      '') (pkgs.lib.attrNames pkgSet.config.hsPkgs.asterius.components.exes)}
      $out/bin/ahc-boot
    '';
  wasm-asterius-ghc = (pkgs.runCommand "wasm-asterius-ghc" {
      version = "0.0.1";
      preferLocalBuild = true;
      passthru = {
        targetPrefix = "wasm-asterius-";
      };
    } ''
      mkdir -p $out/bin
      mkdir -p $out/lib
      ${pkgs.lib.concatMapStringsSep "\n" (exe: ''
        ln -s ${asterius-boot}/bin/${exe} $out/bin/wasm-asterius-ghc${pkgs.lib.strings.substring 3 ((pkgs.lib.strings.stringLength) exe - 3) exe}
      '') (pkgs.lib.attrNames pkgSet.config.hsPkgs.asterius.components.exes)}
      cp -r ${asterius-boot}/boot/.boot/asterius_lib $out/lib/wasm-asterius-ghc-0.0.1
      ln -s ${pkgs.haskell.compiler.${compilerName}}/bin/hsc2hs $out/bin/wasm-asterius-hsc2hs
    '');
in {
  project-nix = stack.nix;
  inherit (pkgSet.config) hsPkgs;
  config = pkgSet.config;
  inherit ghc-head ghc865 pkgs haskell nodejs nodePkgs asterius-boot wasm-asterius-ghc;
  ghc-compiler = pkgs.haskell.compiler.${compilerName};
  ghc-boot-libs = ghc865.boot-libs;
}
