self: super: {
  binaryen = super.binaryen.overrideAttrs (_: rec {
    patches = [
      (self.fetchpatch {
        url =
          "https://patch-diff.githubusercontent.com/raw/WebAssembly/binaryen/pull/3482.patch";
        sha256 = "0s79qw6dm13ralk12wa00cw7az9gd3jvwp904jm1zrdjg6cjp3wa";
      })
    ];
  });
}
