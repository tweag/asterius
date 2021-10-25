# Give your project a name. :)
workspace(name = "bazel_asterius")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
)

git_repository(
    name = "rules_haskell",
    remote = "https://github.com/tweag/rules_haskell.git",
    branch = "master",
)


load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_git_repository",
    "nixpkgs_local_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

# External dependencies specific to asterius
# Based on the nix environemnt used by Stack
nixpkgs_local_repository(
    name = "nixpkgs2",
    nix_file = "//nix:bazel_deps.nix",
    nix_file_deps = ["//nix:default.nix",
                     "//nix:sources.nix",
                     "//nix:binaryenOverlay.nix",
                     "//nix:sources.json"]
)

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
    name = "lib",
    srcs = glob(["lib/*.so", "lib/*.a"]),
    visibility = ["//visibility:public"],
)

cc_library(
    name = "binaryen",
    # srcs = ["@binaryen//:lib"],
    srcs = [":lib"],
    hdrs = [":include"],
    strip_include_prefix = "include",
    visibility = ["//visibility:public"],
    # This rule only bundles headers and a library and doesn't compile or link by itself.
    # We set linkstatic = 1 to quiet to quiet the following warning:
    #
    #   in linkstatic attribute of cc_library rule @zlib.dev//:zlib:
    #   setting 'linkstatic=1' is recommended if there are no object files.
    #
    linkstatic = 1,
)
""",
    repository = "@nixpkgs2",
)


http_archive(
      name = "inline-js-core",
      urls = ["https://github.com/tweag/inline-js/archive/ef675745e84d23d51c50660d40acf9e684fbb2d6.tar.gz"],
      sha256 = "8e69f0f9689efc7217488dc5c5cd66ee3b9f0055a2a7fe655890b7cb57173757", 
      strip_prefix = "inline-js-ef675745e84d23d51c50660d40acf9e684fbb2d6/inline-js-core/",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@stackage//:packages.bzl", "packages")
haskell_cabal_library(
    name = "inline-js-core",
    version = packages["inline-js-core"].version,
    srcs = glob(["**"]),
    deps = packages["inline-js-core"].deps,
    visibility = ["//visibility:public"],
)
    """,
#       build_file_content = """
# filegroup(name = "inline-js-core", srcs = glob(["inline-js-core/**"]))
# """
)


# nixpkgs_package(
#     name = "autoconf",
#     attribute_path = "autoconf",
#     repository = "@nixpkgs2",
# )

# Dependencies for the utils/make_package.sh script
nixpkgs_package(
    name = "nix",
    attribute_path = "nix",
    repository = "@nixpkgs2",
    build_file_content = """
exports_files(["bin/nix-shell"], visibility = ["//visibility:public"])
"""
)

nixpkgs_package(
    name = "cabal",
    attribute_path = "cabal-install",
    repository = "@nixpkgs2",
)

nixpkgs_package(
    name = "ghc",
    attribute_path = "ghc",
    repository = "@nixpkgs2",
)

# Fetch a version of nixpkgs from GitHub.
# For more information see the documentation of rules_nixpkgs at
# https://github.com/tweag/rules_nixpkgs/blob/master/README.md

# This version of nix contains ghc
nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "20.09",
    # sha256 = …
)

[
    nixpkgs_package(
        name = pack,
        repository = "@nixpkgs2",
    )
    for pack in 
    [
        # "haskellPackages.alex"
    "bash",
        # "nix",
        # "cabal-install",
    "stack",
        "autoconf",
        "automake",
        # "binaryen",
    "binutils",
        # "gcc",
        # "haskell.compiler.ghc884",
    "git",
        # "gmp.dev",
        # "haskellPackages.happy",
        # "ncurses.dev",
        # "nodejs-15_x",
     ]
]

nixpkgs_package(
    name = "staasdsadasdck",
    repository = "@nixpkgs2",
)

# https://asterius.netlify.app/building.html#preparing-the-source-tree
# Calls the utils/make_packages.py that generates some packages.
# Then copy the BUILD files from bazel_build_files to these packages

load("//:bazel/make_packages_utils.bzl", "make_packages",)

make_packages(
    name = "make_packages",
    nix_shell_binary = "@nix//:bin/nix-shell",
    nixpkgs = "@nixpkgs",
    nix_shell_file = "//:nix/bazel-nix-shell.nix",
    # tools = [],
    binaries = ["@git//:bin/git", "@stack//:bin/stack", "@autoconf//:bin/autoreconf", "@automake//:bin/aclocal"],

)


load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot"
)

stack_snapshot(
    name = "stackage",
    extra_deps = {
                 "binaryen" : ["@binaryen_dev//:binaryen"]
                  },
    packages = [
        "binaryen",
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
    ],
    verbose = True,
    snapshot = "nightly-2020-09-21",
    vendored_packages = {
        "inline-js-core": "@inline-js-core//:inline-js-core",
                         }
    # This uses an unpinned version of stack_snapshot, meaning that stack is invoked on every build.
    # To switch to pinned stackage dependencies, run `bazel run @stackage-unpinned//:pin` and
    # uncomment the following line.
    # stack_snapshot_json = "//:stackage_snapshot.json",
)



# nixpkgs_package(
#     name ="binaryen_hs",
#     attribute_path = "haskellPackages.binaryen",
#     repository = "@nixpkgs2",
# )

nixpkgs_cc_configure(
    repository = "@nixpkgs",
)

nixpkgs_python_configure(
    repository = "@nixpkgs",
)

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

# Fetch a GHC binary distribution from nixpkgs and register it as a toolchain.
# For more information:
# https://api.haskell.build/haskell/nixpkgs.html#haskell_register_ghc_nixpkgs
haskell_register_ghc_nixpkgs(
    repository = "@nixpkgs",
    attribute_path = "ghc",

    # with @nixpkgs
    version = "8.8.4",
    #version = "8.10.4",
)

# For zlib.BUILD.bazel
nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "zlib.dev",
    build_file = "//:zlib.BUILD.bazel",
    repository = "@nixpkgs",
)


# setup rules pkg to export build artifact
http_archive(
    name = "rules_pkg",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.4.0/rules_pkg-0.4.0.tar.gz",
        "https://github.com/bazelbuild/rules_pkg/releases/download/0.4.0/rules_pkg-0.4.0.tar.gz",
    ],
    sha256 = "038f1caa773a7e35b3663865ffb003169c6a71dc995e39bf4815792f385d837d",
)

load("@rules_pkg//:deps.bzl", "rules_pkg_dependencies")
rules_pkg_dependencies()

# Wasi is a runtime dependency for ahc-link
nixpkgs_package(
    name = "wasilibc",
    attribute_path = "wasi-sdk",
    repository = "@nixpkgs2",
    build_file_content = """
filegroup(name = "wasilibc",
          srcs = glob(["wasi-sdk-12.1g41fa3294474c/**"]),
          visibility = ["//visibility:public"],
)
exports_files(
    glob(["wasi-sdk-12.1g41fa3294474c/bin/*"]),
    visibility = ["//visibility:public"],
)
"""
)


# dependencies for packaging

# for ldd
nixpkgs_package(
    name = "glibc-bin",
    attribute_path = "glibc.bin",
    repository = "@nixpkgs2",
)

nixpkgs_package(
    name = "utillinux",
    repository = "@nixpkgs2",
)

nixpkgs_package(
    name = "tar",
    attribute_path = "gnutar",
    repository = "@nixpkgs2",
)

nixpkgs_package(
    name = "gzip",
    repository = "@nixpkgs2",
)

nixpkgs_package(
    name = "busybox",
    repository = "@nixpkgs2",
)

nixpkgs_package(
    name = "patchelf",
    repository = "@nixpkgs2",
)

http_archive(
    name = "rules_sh",
    sha256 = "83a065ba6469135a35786eb741e17d50f360ca92ab2897857475ab17c0d29931",
    strip_prefix = "rules_sh-0.2.0",
    urls = ["https://github.com/tweag/rules_sh/archive/v0.2.0.tar.gz"],
)
load("@rules_sh//sh:repositories.bzl", "rules_sh_dependencies")
rules_sh_dependencies()

load("@rules_sh//sh:posix.bzl", "sh_posix_configure")
sh_posix_configure()
