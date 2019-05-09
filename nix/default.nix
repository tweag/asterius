{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, ...
}:
let
  # our packages
  # plan-pkgs = import ./pkgs.nix;
  plan-nix = haskell.cabalProjectToNix { src = ./..; };
  plan-pkgs = import "${plan-nix}/nix-plan/pkgs.nix";

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (plan-pkgs.extras {}).compiler or
             (plan-pkgs.pkgs {}).compiler;

  pkgSet = haskell.mkCabalProjectPkgSet {
    inherit plan-pkgs;
    # The extras allow extension or restriction of the set of
    # packages we are interested in. By using the stack-pkgs.extras
    # we restrict our package set to the ones provided in stack.yaml.
    pkg-def-extras = [
      iohk-extras.${compiler.nix-name}
    ];
    modules = [
      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module
      ({ config, ...}: {
          # packages.hsc2hs.components.exes.hsc2hs.doExactConfig = true;
          packages.ghc.patches = [ ./patches/ghc.patch ];
      })
    ];
  };
  asterius-ghc-src-head = haskell.fetchExternal {
    name     = "asterius-ghc-src";
    specJSON = ./ghc-src.json;
    override = "asterius-ghc-src";
  };
  ghc-prim = builtins.fetchTarball {
    url = "http://hackage.haskell.org/package/ghc-prim-0.5.3/ghc-prim-0.5.3.tar.gz";
    sha256 = "1inn9dr481bwddai9i2bbk50i8clzkn4452wgq4g97pcgdy1k8mn";
  };
  testPlan = haskell.cabalProjectToNix { src = ./..; };
  asterius-ghc-src-this = pkgs.srcOnly pkgs.haskell.compiler.${compiler.nix-name};
  asterius-libs-head = pkgs.copyPathToStore ../ghc-toolkit/boot-libs;
  full-ghc-patch = pkgs.copyPathToStore ./patches/ghc/asterius.patch;
  asterius-libs-patch = pkgs.runCommand "asterius-libs-patch" {
    preferLocalBuild = true;
  } ''
    tmp=$(mktemp -d)
    cd $tmp
    ln -s ${asterius-ghc-src-head}/libraries old
    ln -s ${asterius-libs-head} new
    (diff -u -r old/ghc-prim new/ghc-prim | grep -v '^Only in' > $out) || true
  '';
  asterius-boot-libs = pkgs.runCommand "asterius-boot-libs" {
    buildInputs = [];
    preferLocalBuild = true;
  } ''
    set +x
    cp -r ${asterius-ghc-src-this} $out
    chmod +w -R $out
    cd $out
    patch -p1 < ${full-ghc-patch} || true
    cd $out/libraries
    patch -p1 < ${asterius-libs-patch}
    cp ${ghc-prim}/GHC/Prim.hs ghc-prim/GHC/Prim.hs
    cp ${ghc-prim}/GHC/PrimopWrappers.hs ghc-prim/GHC/PrimopWrappers.hs
    sed -i '96,$ d' ghc-prim/ghc-prim.cabal
    mkdir ghc-prim/Asterius
    cp ${asterius-libs-head}/ghc-prim/Asterius/Magic.hs ghc-prim/Asterius/Magic.hs
    mv $out/rts $out/libraries
    cat <<EOF >>$out/libraries/rts/rts.conf
    name: rts
    version: 1.0
    id: rts
    key: rts
    license: BSD-3-Clause
    maintainer: glasgow-haskell-users@haskell.org
    exposed: True
    library-dirs: \$topdir/rts
    hs-libraries: HSrts
    include-dirs: \$topdir/include
    EOF
  '';

in
  pkgSet.config.hsPkgs // {
    _config = pkgSet.config;
    inherit asterius-libs-patch testPlan;
    asterius-boot = pkgs.runCommand "asterius-boot" {
      preferLocalBuild = true;
      nativeBuildInputs = [ pkgs.makeWrapper pkgs.haskell.compiler.${compiler.nix-name}  pkgs.autoconf  ];
    } ''
      mkdir -p $out/bin
      mkdir -p $out/boot
      ${pkgs.lib.concatMapStringsSep "\n" (exe: ''
        makeWrapper ${pkgSet.config.hsPkgs.asterius.components.exes.${exe}}/bin/${exe} $out/bin/${exe} \
          --set asterius_bindir $out/bin \
          --set asterius_bootdir $out/boot \
          --set boot_libs_path ${asterius-boot-libs} \
          --set sandbox_ghc_lib_dir $(ghc --print-libdir)
      '') (pkgs.lib.attrNames pkgSet.config.hsPkgs.asterius.components.exes)}
      $out/bin/ahc-boot
    '';
  }
