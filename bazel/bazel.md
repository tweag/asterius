# The bazel build

## External dependencies to keep in sync.

- The `bazel/stack.yaml` file should be kept in sync with the `nix.cabal.project` files as
we rely on this custom stack snapshot for external haskell dependencies.

On updating this file, the `bazel/stackage_snapshot.json` should be generated again by executing `bazel run @stackage-unpinned//:pin`.

- The `HASKELL_BINARYEN_COMMIT` variable from the `WORKSPACE` file.

Because of a `rules_haskell` [issue](https://github.com/tweag/rules_haskell/issues/1676) we do not recover this library through the stack snapshot.

- The `bazel/nix/bazel_deps.nix` file mimics the `shell.nix` file and exposes necessary attributes to bazel.
It the other nix files are refactored it may be necessary to also modify this one.
