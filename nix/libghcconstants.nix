let
  sources = import ./sources.nix { };
  ghc_asterius = sources.ghc-asterius;
in
self: super: {
  ghcconstants = self.callPackage
    ({ stdenv }:
      stdenv.mkDerivation {
        name = "libghcconstants";
        src = ../ghc-toolkit/cbits;
        installPhase = ''
          mkdir -p $out/lib
          $CC -I${ghc_asterius}/ghc-asterius/autogen -Os -o $out/lib/libghcconstants.so -shared ghc_constants.c
        '';
      })
    { };
}
