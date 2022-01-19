workspace(name = "bazel_asterius")

# This commit must be the same as the one from bazel/stack.yaml

HASKELL_BINARYEN_COMMIT = "7d0ea214e47946fc830d332a6e3e1b73f60f61fd"

######################
# BAZEL DEPENDENCIES #
######################

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
    "new_git_repository",
)

git_repository(
    name = "rules_haskell",
    commit = "26a966abf7328b08c449c682f07d0e00e6f14466",
    remote = "https://github.com/tweag/rules_haskell.git",
    shallow_since = "1641820378 +0000",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
    "nixpkgs_sh_posix_configure",
)

# Setup the posix toolchain
http_archive(
    name = "rules_sh",
    sha256 = "83a065ba6469135a35786eb741e17d50f360ca92ab2897857475ab17c0d29931",
    strip_prefix = "rules_sh-0.2.0",
    urls = ["https://github.com/tweag/rules_sh/archive/v0.2.0.tar.gz"],
)

load("@rules_sh//sh:repositories.bzl", "rules_sh_dependencies")

rules_sh_dependencies()

#####################
# Setup toolchains  #
#####################

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//bazel/nix:bazel_deps.nix",
    nix_file_deps = [
        "//nix:sources.nix",
        "//nix:binaryen.nix",
        "//nix:binaryen.patch",
        "//nix:wasi-sdk.nix",
        "//nix:sources.json",
        "//ghc-toolkit:cbits/ghc_constants.c",
        "//nix:ghcconstants.nix",
    ],
)

nixpkgs_sh_posix_configure(repository = "@nixpkgs")

nixpkgs_cc_configure(repository = "@nixpkgs")

nixpkgs_python_configure(repository = "@nixpkgs")

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path = "ghc",
    repository = "@nixpkgs",
    version = "8.10.7",
)

##############################################
# External dependencies specific to asterius #
##############################################

# We recover the binaryen C library through nix
nixpkgs_package(
    name = "binaryen_dev",
    attribute_path = "binaryen",
    build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")

filegroup(
    name = "include",
    srcs = glob(["include/*.h"]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "binaryen",
    srcs = glob(["lib/*.so", "lib/*.a"]),
    visibility = ["//visibility:public"],
)

cc_library(
    name = "binaryen_cc",
    srcs = [":binaryen"],
    deps = [":binaryen_import"],
    hdrs = [":include"],
    includes = ["include"],
    visibility = ["//visibility:public"],
   )

cc_import(
  name = "binaryen_import",
  hdrs = [":include"],
  shared_library = "lib/libbinaryen.so",
  visibility = ["//visibility:public"],
)

""",
    repository = "@nixpkgs",
)

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    components = {
        "ahc-pkg": ["exe"],
    },
    local_snapshot = "//:bazel/stack.yaml",
    packages = [
        "inline-js-core",
        "base",
        "binary",
        "bytestring",
        "containers",
        "directory",
        "filepath",
        "Cabal",
        "ghc-prim",
        "process",
        "stm",
        "template-haskell",
        "text",
        "haskeline",
        "exceptions",

        # dependencies from ghc-asterius
        # "ahc-bin",
        "ahc-pkg",
        "ghc-asterius",
        "ghc-boot-asterius",
        "ghc-boot-th-asterius",
        "ghc-heap-asterius",
        "ghci-asterius",
        "template-haskell-asterius",
    ],

    # To update the stackage dependencies, run `bazel run @stackage-unpinned//:pin` and
    stack_snapshot_json = "//:bazel/stackage_snapshot.json",
)

# We not use the custom stack snapshot for this in order to work around the following bug.
# https://github.com/tweag/rules_haskell/issues/1676
new_git_repository(
    name = "haskell_binaryen",
    build_file_content = """
load(
    "@rules_haskell//haskell:cabal.bzl",
    "haskell_cabal_library",
)

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
    "haskell_toolchain_library",
)
haskell_toolchain_library(name = "base")
cc_library(
    name = "libbinaryen",
    srcs = ["cbits/wrappers.c"],
    deps = [
            "@binaryen_dev//:binaryen_cc",
    ],
    visibility = ["//visibility:public"]
    )

haskell_library(
    name = "binaryen",
    srcs = glob(["src/**"]),
    deps = [
        "libbinaryen",
        "base",
    ],
    visibility = ["//visibility:public"],
    )
""",
    commit = HASKELL_BINARYEN_COMMIT,
    remote = "https://github.com/tweag/haskell-binaryen",
    shallow_since = "1610373671 +0100",
)

######################
# Tools from nixpkgs #
######################

[
    nixpkgs_package(
        name = pack,
        repository = "@nixpkgs",
    )
    for pack in [
        "hpack",
    ]
]