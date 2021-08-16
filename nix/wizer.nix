self: super: {
  wizer = self.callPackage
    ({ fetchFromGitHub, rustPlatform }:
      rustPlatform.buildRustPackage {
        pname = "wizer";
        version = "1.3.3";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wizer";
          rev = "4881bf6215a9d584947384a0360bc45470285c58";
          sha256 = "sha256-An8798nTst6FcY7j3aJJxtF3Wcdh7fHYZliqN2y00Q0=";
        };
        cargoHash =
          "sha512-dtMtO8sEcyrfSdltlNze31O1FUDd5MvBlS4aMRDWZf9spZB9lSCRPr1BNjOCR6zdkX3ChLD7f3aW179hDzxfNA==";
        cargoBuildFlags = [ "--all-features" ];
        doCheck = true;
        preCheck = "export HOME=$(mktemp -d)";
        hardeningDisable = [ "all" ];
      })
    { };
}
