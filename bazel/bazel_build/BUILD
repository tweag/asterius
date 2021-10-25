load("utils.bzl",
     "asterius_boot",
     "ahc_link",
)
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_toolchain_library",
    "haskell_library",
    "haskell_binary",
    "haskell_repl",
)

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "haskell_toolchain",
)

load("@rules_pkg//:pkg.bzl", "pkg_tar", "pkg_deb")

load("//bazel/bazel_utils/packaging:packaging.bzl", "package_app", "bundle_apps")


asterius_boot(
    name = "asterius_boot",
    srcs = [
        "//asterius:asterius_datadir",
        "launch_ahc_boot_from_utils.sh",
    ],
    tools = [
        "//ghc-toolkit:ghc-toolkit-datafiles",
        "@cabal//:bin",
        "//asterius:ahc",
        "//asterius:ahc-pkg",
        "//asterius:ahc-cabal",
        "//asterius:ahc-dist",
        "//asterius:ahc-ld",
        "//asterius:ahc-link",
        "//asterius:Setup-ghc-prim",
        "//asterius:ahc-boot",
        "@ghc//:bin",
        "@autoconf//:bin",
        "@automake//:bin",
        "@bazel_tools//tools/bash/runfiles",

    ],
    asterius_version = "0.0.1",
)

# ahc_link(
#     name = "test_ahc_link",
#     srcs = [],
#     deps = [],
#     tools = ["//asterius:ahc"],
#     asterius_boot_rule = ":asterius_boot",
#     ahc_link_exe = "//asterius:ahc-link",
# )

# pkg_tar(
#     name = "asterius-output",
#     strip_prefix = "/src",
#     srcs = [
#         ":asterius_boot",
#         "//asterius:ahc",
#         "//asterius:ahc-pkg",
#         "//asterius:ahc-cabal",
#         "//asterius:ahc-dist",
#         "//asterius:ahc-ld",
#         "//asterius:ahc-link",
#     ],
#     mode = "0755",
# )

package_app(
    name = "asterius-out",
    binary = "//asterius:ahc",
    resources = [
        "//bazel/bazel_utils/packaging:package-app.sh",
    ],
    tags = ["no-cache"],
    visibility = ["//visibility:public"],
)


wasi_apps = [
    "ar",
    "clang",
    "clang-12",
    "clang-cl",
    # "clang-format",
    #"git-clang-format", TODO: add a symlink in bundle ?
    "ld64.lld",
    "lld-link",
    "llvm-cxxfilt",
    "llvm-nm",
    "llvm-objdump",
    "llvm-size",
    "llvm-strip",
    "objcopy",
    "ranlib",
    "strings",
    "wasm-ld",
    "c++filt",
    "clang++",
    # "clang-apply-replacements",
    "clang-cpp",
    # "clang-tidy",
    "ld.lld",
    "lld",
    "llvm-ar",
    "llvm-dwarfdump",
    "llvm-objcopy",
    "llvm-ranlib",
    "llvm-strings",
    "nm",
    "objdump",
    "size",
    "strip"]

wasi_sdk_version = "12.1g41fa3294474c"
[
    package_app(
        name = "wasi-sdk-{}".format(app),
        binary = "@wasilibc//:wasi-sdk-{}/bin/{}".format(wasi_sdk_version,  app),
        resources = [
            "//bazel/bazel_utils/packaging:package-app.sh",
            "@wasilibc//:wasilibc",
        ],
        tags = ["no-cache"],
        visibility = ["//visibility:public"],
    )
  for app in wasi_apps
]

bundle_apps(
    name = "wasi_bundle",
    packaged_apps =\
      [":wasi-sdk-{}".format(app) for app in wasi_apps],
    tags = ["no-cache"],
    visibility = ["//visibility:public"],
)

asterius_apps = [
    "ahc",
    "ahc-pkg",
    "ahc-cabal",
    "ahc-dist",
    "ahc-ld",
    "ahc-link",
]

[
    package_app(
        name = "asterius-out-{}".format(app),
        binary = "//asterius:{}".format(app),
        resources = [
            "//bazel/bazel_utils/packaging:package-app.sh",
            "@wasilibc//:wasilibc",
        ],
        tags = ["no-cache"],
        visibility = ["//visibility:public"],
    )
    for app in asterius_apps]

cmds = [
    "mkdir -p $@/bin",
    "cp $(location :wasi_bundle) $@/bin",
    "cd $@/bin",
    "tar xf wasi_bundle.tar.gz",
    "rm wasi_bundle.tar.gz",
    "cd ..",
    "ln -s bin/resources/wasilibc/wasi-sdk-{}/lib lib".format(wasi_sdk_version),
    "ln -s bin/resources/wasilibc/wasi-sdk-{}/share share".format(wasi_sdk_version),
]

genrule(
    name = "extracted_wasi_bundle",
    outs = ["wasi_folder"],
    srcs = [":wasi_bundle"],
    cmd = " && ".join(cmds),
)

genrule(
    name = "tar_wasi_bundle",
    srcs = [":extracted_wasi_bundle"],
    outs = ["tar_wasi_bundle.tar.gz"],
    cmd = "$(location //bazel/bazel_utils/sh:mktgz) $@ -C \"$(location :extracted_wasi_bundle)\" .",
    tools = ["//bazel/bazel_utils/sh:mktgz"],
)

bundle_apps(
    name = "asterius_bundle",
    packaged_apps =\
    [":asterius-out-{}".format(app) for app in asterius_apps] \
    + [":tar_wasi_bundle",
       ":asterius_boot",
       ":wrappers",
       "//bazel/bazel_utils/packaging:asterius_bundle.BUILD.bazel",
       "//bazel/bazel_utils/packaging:wasm_cc_toolchain.bzl",
       ],
    # + [":wasi-sdk-{}".format(app) for app in wasi_apps],
    tags = ["no-cache"],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "wrappers",
    srcs = glob(["wrappers/*"])
)
