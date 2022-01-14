# This rule enables to mix the generated cabal file produced by hpack
# with the rest of the source files.

load("@bazel_skylib//lib:paths.bzl", "paths")

def _cabal_filegroup_impl(ctx):
    f = ctx.file.generated_cabal_file
    outputs = []
    for g in ctx.files.srcs:
        gl = ctx.actions.declare_symlink(
            paths.relativize(g.path, ctx.attr.prefix_path),
        )
        ctx.actions.symlink(output = gl, target_path = g.path)
        outputs.append(gl)
    return [
        DefaultInfo(files = depset(outputs + [f])),
    ]

cabal_filegroup = rule(
    _cabal_filegroup_impl,
    attrs = {
        "srcs": attr.label_list(
            allow_files = True,
        ),
        "prefix_path": attr.string(),
        "generated_cabal_file": attr.label(
            allow_single_file = True,
        ),
    },
)
