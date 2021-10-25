# Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

"""Packaging of Linux, macOS and Windows binaries into tarballs"""

# load("@os_info//:os_info.bzl", "is_windows")

def _bundle_apps_impl(ctx):
    args = ctx.actions.args()
    args.add(ctx.outputs.out.path)
    args.add_all(ctx.attr.packaged_apps, map_each = _get_resource_path)
    posix_info = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]
    ctx.actions.run(
        executable = ctx.executable.bundle_apps,
        outputs = [ctx.outputs.out],
        inputs = ctx.files.packaged_apps,
        tools = [],
        arguments = [args],
        progress_message = "Packaging " + ctx.attr.name,
        env = {"PATH": ":".join(posix_info.paths)},
    )

def _package_app_impl(ctx):
    args = ctx.actions.args()
    args.add(ctx.executable.binary.path)
    args.add(ctx.outputs.out.path)
    args.add_all(ctx.attr.resources, map_each = _get_resource_path)
    posix_info = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]
    ctx.actions.run(
        executable = ctx.executable.package_app,
        outputs = [ctx.outputs.out],
        inputs = ctx.files.resources,
        # Binaries are passed through tools so that Bazel can make the runfiles
        # tree available to the action.
        tools = [ctx.executable.binary],
        arguments = [args],
        progress_message = "Packaging " + ctx.attr.name,
        env = {"PATH": ":".join(posix_info.paths)},
    )

def _get_resource_path(r):
    """Get the path to use for a resource.
      If the resource has a single file, it'll be copied to
      the resource root directory. With multiple files the
      relative directory structure is preserved.

      This mirrors how rules that produce directories work
      in Buck.
    """
    files = r.files.to_list()
    if len(files) > 1:
        first_file = files[0].path
        prefix_end = first_file.index(r.label.package)
        # e.g. package foo/bar,
        # first file at bazel-out/k8-fastbuild/bleh/foo/bar/baz/quux
        # return path as bazel-out/k8-fastbuild/bleh/foo/bar.
        return first_file[0:(prefix_end + len(r.label.package) + len(r.label.workspace_root))]
    else:
        return files[0].path

package_app = rule(
    implementation = _package_app_impl,
    attrs = dict({
        "binary": attr.label(
            cfg = "target",
            executable = True,
            allow_files = True,
        ),
        "resources": attr.label_list(
            allow_files = True,
        ),
        "package_app": attr.label(
            default = Label("//bazel/bazel_utils/packaging:package-app"),
            cfg = "host",
            executable = True,
            allow_files = True,
        ),
    }),
    outputs = {
        "out": "%{name}.tar.gz",
    },
    toolchains = ["@rules_sh//sh/posix:toolchain_type"],
)
"""Package a binary along with its dynamic library dependencies and data dependencies
  into a tarball. The data dependencies are placed in 'resources' directory.
"""


bundle_apps = rule(
    implementation = _bundle_apps_impl,
    attrs = dict({
        "packaged_apps": attr.label_list(
            cfg = "target",
            allow_files = True,
        ),
        "bundle_apps": attr.label(
            default = Label("//bazel/bazel_utils/packaging:bundle-apps"),
            cfg = "exec",
            executable = True,
            allow_files = True,
        ),
    }),
    outputs = {
        "out": "%{name}.tar.gz",
    },
    toolchains = ["@rules_sh//sh/posix:toolchain_type"],
)
"""
Bundle packages created by the package_app rule into one tarball.
"""
