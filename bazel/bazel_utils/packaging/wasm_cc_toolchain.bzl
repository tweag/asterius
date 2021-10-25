load("@bazel_skylib//lib:paths.bzl", "paths")

load(
    "@bazel_tools//tools/build_defs/cc:action_names.bzl",
    "ACTION_NAMES",
)

load(
    "@bazel_tools//tools/cpp:cc_toolchain_config_lib.bzl",
    "tool_path",
    "feature",
    "flag_group",
    "flag_set",
)

def _impl(ctx):
    # posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]
    # ar_path = posix.commands["ar"]
    tool_paths = [
        tool_path(
            name = "gcc",
            path = "bin/clang",
        ),
        tool_path(
            name = "ld",
            path = "bin/wasm-ld",
        ),
        tool_path(
            name = "ar",
            path = "bin/ar",
        ),
        tool_path(
            name = "cpp",
            path = "bin/clang++",
        ),
        tool_path(
            name = "gcov",
            path = "gcov_not_supported.sh",
        ),
        tool_path(
            name = "nm",
            path = "bin/nm",
        ),
        tool_path(
            name = "objdump",
            path = "bin/objdump",
        ),
        tool_path(
            name = "strip",
            path = "bin/strip",
        ),
    ]

    sysroot = paths.dirname(paths.dirname(ctx.file.file_from_wasi_sysroot_include.path))
    print("sysroot =", sysroot)
    asterius_include = paths.dirname(ctx.file.file_from_asterius_libs_include.path)
    default_flags = feature(
        name = "default_flags",
        enabled = True,
        flag_sets = [
            flag_set(
                actions = [
                    #ACTION_NAMES.c_compile,
                    #ACTION_NAMES.cpp_compile,
                    # ACTION_NAMES.linkstamp_compile
                    # ACTION_NAMES.cc_flags_make_variable
                    # ACTION_NAMES.cpp_module_codegen
                    # ACTION_NAMES.cpp_header_parsing
                    # ACTION_NAMES.cpp_module_compile
                    # ACTION_NAMES.assemble
                    # ACTION_NAMES.preprocess_assemble
                    # ACTION_NAMES.llvm_cov
                    # ACTION_NAMES.lto_indexing 
                    # ACTION_NAMES.lto_backend 
                    # ACTION_NAMES.lto_index_for_executable 
                    # ACTION_NAMES.lto_index_for_dynamic_library 
                    # ACTION_NAMES.lto_index_for_nodeps_dynamic_library 
                    # ACTION_NAMES.cpp_link_executable 
                    # ACTION_NAMES.cpp_link_dynamic_library 
                    # ACTION_NAMES.cpp_link_nodeps_dynamic_library 
                    # ACTION_NAMES.cpp_link_static_library 
                    # ACTION_NAMES.strip 
                    # ACTION_NAMES.objc_archive 
                    # ACTION_NAMES.objc_compile 
                    # ACTION_NAMES.objc_executable 
                    # ACTION_NAMES.objc_fully_link 
                    # ACTION_NAMES.objcpp_compile 
                    # ACTION_NAMES.objcpp_executable 
                    # ACTION_NAMES.clif_match 
                    ACTION_NAMES.assemble,
                    ACTION_NAMES.preprocess_assemble,
                    ACTION_NAMES.linkstamp_compile,
                    ACTION_NAMES.c_compile,
                    ACTION_NAMES.cpp_compile,
                    ACTION_NAMES.cpp_header_parsing,
                    ACTION_NAMES.cpp_module_compile,
                    ACTION_NAMES.cpp_module_codegen,
                    ACTION_NAMES.lto_backend,
                    ACTION_NAMES.clif_match,
                ],
                flag_groups = [
                    flag_group(
                        flags = [
                            # "-lstdc++",
                            "--sysroot={}".format(sysroot),
                            "-I{}".format(asterius_include),
                            # "-Oz",
                            "-flto",
                            "-no-canonical-prefixes",
                            "-isystem",
                            "{}/lib/clang/12.0.0/include".format(paths.dirname(paths.dirname(sysroot)))
                        ],
                    ),
                ],
            ),
        ],
    )
    return cc_common.create_cc_toolchain_config_info(
        ctx = ctx,
        toolchain_identifier = "webassembly_bundle",
        host_system_name = "local",
        target_system_name = "wasm64",
        target_cpu = "wasm64",
        target_libc = "unknown",
        compiler = "wasi",
        abi_version = "unknown",
        abi_libc_version = "unknown",
        tool_paths = tool_paths,
        features = [default_flags],
        builtin_sysroot = sysroot,
  #       cxx_builtin_include_directories = [
  #           "%sysroot%/lib/clang/12.0.0/include",
  #           "lib/clang/12.0.0/include",

  #          # "external/asterius_bundle_rules_haskell_ghc_nixpkgs_haskell_toolchain/bin/lib/clang/12.0.0/include/",
  #          # "%package(@asterius_bundle_rules_haskell_ghc_nixpkgs_haskell_toolchain//)%/bin/lib/clang/12.0.0/include",
  #          # "%package(//)%/bin/lib/clang/12.0.0/include",
  #          # "%package(@asterius_bundle_rules_haskell_ghc_nixpkgs_haskell_toolchain//bin/lib/clang/12.0.0/include)%",
  #          # "/home/stan/.cache/bazel/_bazel_stan/3c7d084a53b480b3a5081e4bf80f09ce/external/asterius_bundle_rules_haskell_ghc_nixpkgs_haskell_toolchain/bin/lib/clang/12.0.0/include",
      
  # ]
    ) #+ [platform_common.ToolchainInfo(name = "dummy_cpp_toolchain")]
      

cc_toolchain_config = rule(
    implementation = _impl,
    attrs = {
        "file_from_asterius_libs_include": attr.label(
            mandatory = True,
            allow_single_file = True,
            doc = "a file of the asterius libs include folder, to recover the path of this folder",
        ),
        "file_from_wasi_sysroot_include": attr.label(
            mandatory = True,
            allow_single_file = True,
            doc = "a file from the wasi sysroot include folder, to recover the path of this folder",
        )
    },
    provides = [CcToolchainConfigInfo],
    # toolchains = [
    #     "@rules_sh//sh/posix:toolchain_type",
    # ],
)
