{ haskell-nix, mdbook, stdenvNoCC, stix-two, texlive }:
stdenvNoCC.mkDerivation {
  name = "asterius-docs";
  src = haskell-nix.haskellLib.cleanGit {
    name = "asterius-docs-src";
    src = ../.;
    subDir = "docs";
  };
  nativeBuildInputs = [ mdbook stix-two texlive.combined.scheme-full ];
  buildPhase = ''
    pushd $(mktemp -d)
    latexmk -pdfxe -file-line-error -halt-on-error $OLDPWD/src/semantics.tex
    mv semantics.pdf $OLDPWD/src
    popd
    mdbook build --dest-dir $out
  '';
  dontInstall = true;
  allowedReferences = [ ];
}
