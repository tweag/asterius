self: super: {
  wizer = self.callPackage
    ({ fetchFromGitHub, rustPlatform }:
      rustPlatform.buildRustPackage {
        pname = "wizer";
        version = "1.3.3";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wizer";
          rev = "9a107c574cfef10ea5c39483202ddee716c6636e";
          sha256 = "sha256-6g6DtpWSZzFe5BsqMn2Wz/z7aUl2UF9F9ECAFypjOlQ=";
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
