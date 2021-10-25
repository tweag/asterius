({ sources ? import ./sources.nix {}
, config
, overlays
, pkgs ? import sources.nixpkgs {
    overlays = [
      (import ./binaryenOverlay.nix)
    ];
  }
 }:

with pkgs;
let wasi-sdk = stdenv.mkDerivation rec {
  # wasi-sdk is a runtime dependency of asterius

  name = "wasi-sdk${version}";
  version = "12.1";
  src = fetchurl {
    urls = ["https://github.com/tweag/wasi-sdk/releases/download/210113/wasi-sdk-12.1g41fa3294474c-linux.tar.gz"];
    sha256 = "0ncyv3c6v9klcws87q206mx0s66yph7yjqrzlkayamkdidcxkg80";
    
  };

  nativeBuildInputs = [
    unzip
    autoPatchelfHook

  ];

  buildInputs = [
    stdenv.cc.cc.lib
  ];

  unpackPhase = ''
    tar xf $src
  '';

  installPhase = ''
    mkdir -p $out && cp -r * $out/
  '';

  meta = with lib; {
    description = "wasi-sdk";
    platforms = platforms.linux;
  };
}; in
(pkgs//{wasi-sdk = wasi-sdk;})
)
