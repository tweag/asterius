{ pkgs ? import nixpkgs ((haskellNix.nixpkgsArgs) // (if system == null then {} else { inherit system; }))
# Use a pinned nixpkgs rather than the one on NIX_PATH
, haskellNix ? import (builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/a39b5a741acca2fb4bfbb40251d984b2971287ee.tar.gz";
    sha256 = "1p4932iryai8ifz70d30bvswn5g50hxwblizlibjx9s280l1h6k1";
  }) {}
, nixpkgs ? haskellNix.sources.nixpkgs-default
, shellOnly ? false
, system    ? null
}:
let
  cabalPatch = pkgs.fetchpatch {
    url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
    sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
    stripLen = 1;
  };

  # Node
  nodejs = pkgs.nodejs-12_x;
  nodePkgs = import ./nix/node { inherit pkgs nodejs; };

  compilerName = "ghc865";
  # project = stack;
  # mkProjectPkgSet = args: haskell.mkStackPkgSet (args // { stack-pkgs = stack.pkgs; });
  # project = plan;
  # mkProjectPkgSet = args: haskell.mkCabalProjectPkgSet (args // { plan-pkgs = plan.pkgs; });
  
  project = pkgs.haskell-nix.cabalProject' {
    name = "asterius";
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "asterius"; };
    pkg-def-extras = [ pkgs.ghc-boot-packages.ghc865 ];
    modules = [
      { reinstallableLibGhc = true; }
      ({ config, ...}: {
        packages = {
          ghc.patches = [ ./nix/patches/ghc.patch ];
          Cabal.patches = [ cabalPatch ];
          haddock-api.components.library.doHaddock = false;
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
             }) (pkgs.haskell-nix.cabalProject' {
               name = "asterius-tests";
               src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
               pkg-def-extras = [ pkgs.ghc-boot-packages.ghc865 ];
               modules = [];
             }).hsPkgs.asterius.components.tests;
        };
      })
    ];
  };
  # Patch file that can be applied to the full ghc tree
  # full-ghc-patch = pkgs.copyPathToStore ./nix/patches/ghc/asterius.patch;
  ghc-head = let
    # Only gitlab has the right submoudle refs (the ones in github mirror do not work)
    # and only fetchgit seems to get the submoudles from gitlab
    ghc-src = pkgs.srcOnly pkgs.haskell-nix.compiler.ghc865;
    ghc-prim = pkgs.fetchzip {
      url = "https://hackage.haskell.org/package/ghc-prim-0.5.3/ghc-prim-0.5.3.tar.gz";
      sha256 = "1inn9dr481bwddai9i2bbk50i8clzkn4452wgq4g97pcgdy1k8mn";
    };
    # The patched libs are currently in the repo
    boot-libs = pkgs.copyPathToStore ./ghc-toolkit/boot-libs;
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
    ghc-src = pkgs.haskell-nix.compiler.ghc865.passthru.configured-src;
    ghc-prim = pkgs.fetchzip {
      url = "https://hackage.haskell.org/package/ghc-prim-0.5.3/ghc-prim-0.5.3.tar.gz";
      sha256 = "1inn9dr481bwddai9i2bbk50i8clzkn4452wgq4g97pcgdy1k8mn";
    };
    patch = pkgs.copyPathToStore ./nix/patches/ghc/ghc865-libs.patch;
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
      buildInputs = [ pkgs.haskell-nix.compiler.${compilerName} ];
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
      nativeBuildInputs = [ pkgs.makeWrapper pkgs.haskell-nix.compiler.${compilerName} pkgs.autoconf pkgs.automake ];
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
      cp -r ${./ghc-toolkit/ghc-libdir}/include/* $out/ghc-libdir/include
      ${pkgs.lib.concatMapStringsSep "\n" (exe: ''
        makeWrapper ${project.hsPkgs.asterius.components.exes.${exe}}/bin/${exe} $out/bin/${exe} \
          --prefix PATH : ${nodePkgs.parcel-bundler}/bin \
          --set asterius_bindir $out/bin \
          --set asterius_bootdir $out/boot \
          --set boot_libs_path ${ghc865.boot-libs} \
          --set sandbox_ghc_lib_dir $out/ghc-libdir
      '') (pkgs.lib.attrNames project.hsPkgs.asterius.components.exes)}
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
      '') (pkgs.lib.attrNames project.hsPkgs.asterius.components.exes)}
      cp -r ${asterius-boot}/boot/.boot/asterius_lib $out/lib/wasm-asterius-ghc-0.0.1
      ln -s ${pkgs.haskell-nix.compiler.${compilerName}}/bin/hsc2hs $out/bin/wasm-asterius-hsc2hs
    '');

  cabalSystem = builtins.replaceStrings ["-darwin"] ["-osx"] pkgs.stdenv.system;

  # Use this to set the version of asterius to be booted in the shell.
  # By pinning this we avoid re running ahc-boot for every change.
  cached = import ./. {}; # Pin an old commit once stuff works again
  #pkgs.fetchgit {
  #  url = "https://github.com/input-output-hk/asterius";
  #  rev = "572b17398602a435650d7409cc7f00d1dd278eda";
  #  sha256 = "153qa86jcr4zl8haxdqrjp96v8mmv4r5w4p8b8cclic619cklidm";
  #  fetchSubmodules = true;
  #}) {};
  ghc-compiler = pkgs.haskell-nix.compiler.${compilerName};
  shells = {
    ghc = (project.hsPkgs.shellFor {
      # Shell will provide the dependencies of asterius, but not asterius itself.
      packages = ps: with ps; [
        asterius
        binaryen
        ghc-toolkit
        wabt
        inline-js
        inline-js-core
        wabt
        wasm-toolkit ];
    }).overrideAttrs (oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++ [
        project.hsPkgs.hpack.components.exes.hpack
        pkgs.wabt
        pkgs.cmake
        nodejs
        nodePkgs.parcel-bundler
        nodePkgs.todomvc-app-css
        nodePkgs.todomvc-common ];
      shellHook = (oldAttrs.shellHook or "") + ''
        unset CABAL_CONFIG
        export asterius_bootdir=${cached.asterius-boot}/boot
        find . -name package.yaml -exec hpack "{}" \;
        export asterius_datadir=$(pwd)/asterius
        export binaryen_datadir=$(pwd)/binaryen
        export ghc_toolkit_datadir=$(pwd)/ghc-toolkit
        # export sandbox_ghc_lib_dir=$(ghc --print-libdir) # does not include `include` dir
        export sandbox_ghc_lib_dir=$(${ghc-compiler}/bin/ghc --print-libdir)
        export inline_js_datadir=$(pwd)/inline-js/inline-js
        export inline_js_core_datadir=$(pwd)/inline-js/inline-js-core
        export wabt_datadir=$(pwd)/wabt
        export wasm_toolkit_datadir=$(pwd)/wasm-toolkit
        export boot_libs_path=${ghc865.boot-libs}
        mkdir -p asterius-cabal-bin
        cd asterius-cabal-bin
        export asterius_bindir=$(pwd)
        export PATH=$(pwd):$PATH
        ''
        + pkgs.lib.concatMapStrings (exe: ''
          ln -sf ../dist-newstyle/build/${cabalSystem}/ghc-8.6.5/asterius-0.0.1/build/${exe}/${exe} ${exe}
        '') ["ahc" "ahc-boot" "ahc-cabal" "ahc-dist" "ahc-ld" "ahc-link" "ahc-pkg"]
        + ''
        cd ..
      '';
    });
  };
in project // {
  inherit ghc-head ghc865 pkgs nodejs nodePkgs asterius-boot wasm-asterius-ghc shells cached;
  ghc-boot-libs = ghc865.boot-libs;
}
