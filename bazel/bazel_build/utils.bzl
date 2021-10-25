load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:new_sets.bzl", "sets")
load(
    "@rules_haskell//haskell:providers.bzl",
    "HaddockInfo",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "all_dependencies_package_ids",
)

load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")

# boot_dir = paths.join(data_dir, ".boot")
# boot_env = {"ASTERIUS_BOOT_LIBS_DIR": paths.join(data_dir, "boot-libs"), 
#             "ASTERIUS_SANDBOX_GHC_LIBDIR": paths.join(data_dir, "ghc-libdir"),
#             "ASTERIUS_LIB_DIR": paths.join(boot_dir,"asterius_lib"),
#             "ASTERIUS_TMP_DIR": bootTmpDir args,
#             "ASTERIUS_AHCPKG": ahc_pkg_path,
#             "ASTERIUS_SETUP_GHC_PRIM": setupGhcPrim,
#             "ASTERIUS_CONFIGURE_OPTIONS": configureOptions
#             }

def paths_of_tools(ctx):
    path = sets.make()
    for d in ctx.attr.tools:
        if HaskellInfo in d and HaskellLibraryInfo not in d:
            # haskell binary rule
            for f in d.files.to_list():
                sets.insert(path, paths.dirname(f.path))

        if d.label.name == "bin":
            for f in d.files.to_list():
                sets.insert(path, paths.dirname(f.path))

    path_string = " ".join(sets.to_list(path))

    return {"PATH_BZL": path_string}


def _asterius_boot_impl(ctx):
    data_dir = ctx.actions.declare_directory(
        "asterius-{}_data".format(ctx.attr.asterius_version),
    )

    tools = depset(transitive = [t.files for t in ctx.attr.tools])
    inputs = depset(transitive = [t.files for t in ctx.attr.srcs])

    cc_toolchain = find_cpp_toolchain(ctx)
    cc_bin_path = paths.dirname(cc_toolchain.compiler_executable)
    haskell_toolchain =  ctx.toolchains["@rules_haskell//haskell:toolchain"]

    for f in haskell_toolchain.bindir:
        haskell_bin_path = paths.dirname(f.path)

    ctx.actions.run_shell(
        tools = tools,
        inputs = inputs,
        outputs = [data_dir],
        command = "PATH={}:{}:$PATH bazel/bazel_build/launch_ahc_boot_from_utils.sh $1".format(cc_bin_path, haskell_bin_path),

        env = paths_of_tools(ctx),
        arguments = [data_dir.path],
    )

    # ctx.actions.run_shell(
    #     tools = tools,
    #     inputs = [],
    #     command = "PATH={}:$PATH ln -s $(ahc_pkg field base haddock-html --simple-output) docdir".format(haskell_bin_path),

    # )
    default_info = DefaultInfo(files = depset([data_dir]))
    return [default_info]

asterius_boot = rule(
    _asterius_boot_impl,
    attrs = {
        "srcs": attr.label_list(
            allow_files = True,
            doc = "",
        ),
        "tools": attr.label_list(),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
        "asterius_version": attr.string(),
    },
    toolchains = [
        "@rules_haskell//haskell:toolchain",
        "@bazel_tools//tools/cpp:toolchain_type",
                  ] ,

    )


def _ahc_link_impl(ctx):
    ahc_link_executable = ctx.attr.ahc_link_exe.files
    asterius_boot_rule = ctx.file.asterius_boot_rule
    ahc_link_executable_path = ahc_link_executable.to_list()[0]
    source_files = ctx.attr.srcs
    output_file = ctx.actions.declare_file(
        "output_file_name"
    )
    tools = depset(direct = [asterius_boot_rule], transitive = [t.files for t in ctx.attr.tools]+[ahc_link_executable])
    ctx.actions.run(
        tools = tools,
        inputs = source_files,
        outputs = [output_file],
        executable = ahc_link_executable_path,
        env = {"asterius_datadir": asterius_boot_rule.path} ,
        arguments = ["--input-hs"] + [s.path for s in source_files] + ["--browser", "--bundle"],
        # arguments = ["--input-hs"+" ".join([s.path for s in source_files]) + "--browser --bundle"],
    )
    default_info = DefaultInfo(
        files = depset([output_file])
    )
    return [default_info]

ahc_link = rule(
    _ahc_link_impl,
    attrs = {
        "srcs": attr.label_list(),
        "deps": attr.label_list(),
        "tools": attr.label_list(),
        "asterius_boot_rule": attr.label(allow_single_file = True),
        "ahc_link_exe": attr.label(),
    },
)
