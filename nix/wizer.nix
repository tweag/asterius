self: super: {
  wizer = self.callPackage
    ({ fetchFromGitHub, rustPlatform }:
      rustPlatform.buildRustPackage {
        pname = "wizer";
        version = "1.3.3";
        src = fetchFromGitHub {
          owner = "bytecodealliance";
          repo = "wizer";
          rev = "953e04671727d8a60d5f61d91f6d3ed91da0689d";
          sha256 = "sha256-/2dTxANBmA7QQKzyYyajDbT4ozlo3nUJ2gXfuMAx0dk=";
        };
        cargoHash =
          "sha512-i2aC9fTNMGnlGAAF9X33Vgp9UxAQ8KUrBLWIJUNWF+eWqmzV9q6af48Nx8L6E/j0JrcIT/TyNBSpqBc5dXntsg==";
        cargoBuildFlags = [ "--all-features" ];
        doCheck = true;
        preCheck = "export HOME=$(mktemp -d)";
      })
    { };
}
