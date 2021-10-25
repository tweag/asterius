self: super:
{
  binaryen = super.binaryen.overrideAttrs (_: {
    version = "99";
    src = self.fetchFromGitHub {
      owner = "WebAssembly";
      repo = "binaryen";
      rev = "version_99";
      sha256 = "1a6ixxm1f8mrr9mn6a0pimajdzsdr4w1qhr92skxq67168vvc1ic";
    };
    patches = [
      (self.fetchpatch {
        url = "https://patch-diff.githubusercontent.com/raw/WebAssembly/binaryen/pull/3482.patch";
        sha256 = "0s79qw6dm13ralk12wa00cw7az9gd3jvwp904jm1zrdjg6cjp3wa";
      })
    ];
  });
}
