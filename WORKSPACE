workspace(name = "io_tweag_asterius")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-c625c53c3fe6644abb366bd868051d9e82f2a204",
    urls = ["https://github.com/tweag/rules_haskell/archive/c625c53c3fe6644abb366bd868051d9e82f2a204.tar.gz"],
    sha256 = "73f822145467f03e11996e518191036c307715b53dd879b125618c23c4971174",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

nixpkgs_local_repository(
    name = "nixpkgs_default",
    nix_file = "//nixpkgs:default.nix",
)

nixpkgs_package(
    name = "glibc_locales",
    attribute_path = "glibcLocales",
    build_file_content = """
filegroup(
    name = "locale-archive",
    srcs = ["lib/locale/locale-archive"],
    visibility = ["//visibility:public"],
)
""",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "binaryen",
    build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "binaryen",
    srcs = glob(["lib/**/*.so*", "lib/**/*.dylib", "lib/**/*.a"]),
    hdrs = glob(["include/*.h"]),
    strip_include_prefix = "include",
    visibility = ["//visibility:public"],
)
    """,
    repository = "@nixpkgs_default",
)

nixpkgs_cc_configure(repository = "@nixpkgs_default")

nixpkgs_python_configure(repository = "@nixpkgs_default")

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

haskell_register_ghc_nixpkgs(
    attribute_path = "",
    nix_file = "//nixpkgs/ghc:default.nix",
    nix_file_deps = [
        "//nixpkgs/ghc:0001-D5082.patch",
        "//nixpkgs/ghc:0002-asterius.patch",
        "//nixpkgs/ghc:0003-asterius-iserv.patch",
        "//nixpkgs/ghc:0004-expose-internals.patch",
    ],
    locale_archive = "@glibc_locales//:locale-archive",
    repository = "@nixpkgs_default",
    version = "8.8.3",
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    extra_deps = {"binaryen": ["@binaryen"]},
    packages = [
        "base",
        "bazel-runfiles",
        "binary",
        "binaryen",
        "bytestring",
        "Cabal",
        "containers",
        "deepseq",
        "directory",
        "filepath",
        "ghc",
        "ghc-boot",
        "ghc-prim",
        "ghci",
        "inline-js-core",
        "libiserv",
        "mtl",
        "process",
        "stm",
        "template-haskell",
        "transformers",
    ],
    local_snapshot = "//:snapshot.yaml",
)
