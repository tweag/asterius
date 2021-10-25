

## Local settings
 - In `.bazelrc`:

```
build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host
run --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host

build --sandbox_writable_path=/home/stan/.ahc-cabal
run --sandbox_writable_path=/home/stan/.ahc-cabal

```

 - In `bazel_utils/make_packages_wrapper.sh`:

```
export GIT_SSL_CAINFO="/etc/ssl/certs/ca-certificates.crt"
```


## WIP
This is a WIP, so there is some cleanup to do:

 - Some slight modifications of Asterius source code should be removed.

 - The rules related to the boot process are in the `test` repository.
   They should be moved out and this directory should be added back to .gitignore


## TODO

- Bazel makes calls to calls to `utils/make-packages.py` and `ahc-boot`.
  It would more fine grained to use Bazel itself instead.

- Only tested on NixOS.

- Add rules to export the artifacts.

- Bazel uses `haskell_cabal_library` and `haskell_cabal_binary` to build the various target. This also could be finer grained.
