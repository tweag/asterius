load("@bazel_skylib//lib:new_sets.bzl", "sets")
load("@bazel_skylib//lib:paths.bzl", "paths")

def paths_of_tools(repository_ctx):
    path = sets.make()
    ld_library_path = sets.make()
    for label in repository_ctx.attr.binaries:
        sets.insert(path, str(repository_ctx.path(label).dirname))

    path_string = ":".join(sets.to_list(path))

    print("end path = ", path_string)
    return {"PATH_BZL": path_string}

def _make_packages_impl(repository_ctx):

  # bash
  bash_path = repository_ctx.path(repository_ctx.attr._bash)

  # nix
  nix_folder = repository_ctx.path(repository_ctx.attr.nix_shell_file).dirname
  repository_ctx.symlink(nix_folder, "nix")

  utils_folder = "{}/utils".format(nix_folder.dirname)
  repository_ctx.symlink(utils_folder, "utils")

  nix_shell_executable = repository_ctx.path(repository_ctx.attr.nix_shell_binary)
  nix_path = repository_ctx.path(repository_ctx.attr.nixpkgs).dirname

  # repository_ctx.symlink("/home/stan/src/asterius/shell.nix", "shell.nix")
  make_packages_path = repository_ctx.path("utils/make-packages.py")
  make_packages_wrapper = repository_ctx.path(repository_ctx.attr._make_packages_wrapper)
  # cmd = "NIX_PATH={} {}".format(nix_path, make_packages_path)

  # set_path_path = repository_ctx.path(repository_ctx.attr._set_path)

  cmd = "{} {}".format(make_packages_wrapper, make_packages_path)

  out = repository_ctx.execute(
      # [nix_shell_executable, "nix/bazel-nix-shell.nix", "--pure", "--run", cmd],
      [bash_path, "-v", "-c", cmd],
      timeout=600,
      quiet=False,
      working_directory="",
      environment = dict(paths_of_tools(repository_ctx), NIX_PATH = str(nix_path))
  )
  print("stderr =", out.stderr)
  print("stdout =", out.stdout)
  print("return code =", out.return_code)
  if out.return_code != 0:
      fail("call to make_packages failed")


  build_files_folder = "{}/bazel/bazel_build_files".format(nix_folder.dirname)
  build_directories = [
      "ghc-asterius",
      "ghc-boot-th-asterius",
      "ghc-boot-asterius",
      "ghc-heap-asterius",
      "ghci-asterius",
      "template-haskell-asterius",
  ]
  for d in build_directories:
    repository_ctx.symlink("{}/BUILD_{}".format(build_files_folder, d), "{}/BUILD.bazel".format(d))

make_packages = repository_rule(
    implementation=_make_packages_impl,
    local=False,
    attrs={
        "nix_shell_binary": attr.label(allow_single_file = True, mandatory=True),
        "nixpkgs":attr.label(allow_single_file = True, mandatory=True),
        "nix_shell_file": attr.label(allow_single_file = True, mandatory=True),
        "_make_packages_wrapper":attr.label(
            allow_single_file = True,
	    default = "//:bazel/bazel_utils/make_packages_wrapper.sh"),
        "_bash": attr.label(
            allow_single_file = True,
            default = "@bash//:bin/bash"
        ),
        "binaries": attr.label_list(),
    }
)
