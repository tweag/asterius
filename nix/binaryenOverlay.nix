let
  sources = import ./sources.nix { };
in
self: super:
{
  binaryen = super.binaryen.overrideAttrs (oldAttrs: {
    version = "98";
    src = sources.binaryen;
    patches = [ ];
  });
}
