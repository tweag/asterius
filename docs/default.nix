{ haskell-nix, mdbook, stdenvNoCC }:
stdenvNoCC.mkDerivation {
  name = "asterius-docs";
  src = haskell-nix.haskellLib.cleanGit {
    name = "asterius-docs-src";
    src = ../.;
    subDir = "docs";
  };
  nativeBuildInputs = [ mdbook ];
  buildPhase = ''
    mdbook build --dest-dir $out
  '';
  dontInstall = true;
  allowedReferences = [ ];
}
