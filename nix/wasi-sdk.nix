{ sources ? import ./sources.nix { }
, ghcAsteriusSources ? import "${sources.ghc-asterius}/nix/sources.nix" { }
}:
ghcAsteriusSources.wasi-sdk
