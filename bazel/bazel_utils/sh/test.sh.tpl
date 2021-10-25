#!/usr/bin/env bash
# Copy-pasted from the Bazel Bash runfiles library v2.
set +e
set -uo pipefail; f=bazel_tools/tools/bash/runfiles/runfiles.bash
source "${RUNFILES_DIR:-/dev/null}/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "${RUNFILES_MANIFEST_FILE:-/dev/null}" | cut -f2- -d' ')" 2>/dev/null || \
  source "$0.runfiles/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.exe.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  { echo>&2 "ERROR: cannot find $f"; exit 1; }; f=; set -e
# --- end runfiles.bash initialization v2 ---
canonicalize_rlocation() {
  # Note (MK): This is a fun one: Let’s say $TEST_WORKSPACE is "compatibility"
  # and the argument points to a target from an external workspace, e.g.,
  # @daml-sdk-0.0.0//:daml. Then the short path will point to
  # ../daml-sdk-0.0.0/daml. Putting things together we end up with
  # compatibility/../daml-sdk-0.0.0/daml. On Linux and MacOS this works
  # just fine. However, on windows we need to normalize the path
  # or rlocation will fail to find the path in the manifest file.
  rlocation $(realpath -L -s -m --relative-to=$PWD $TEST_WORKSPACE/$1)
}
get_exe() {
  # Usage: get_exe FILE...
  #
  # On Windows: Return the FILE ending on .exe or the first argument.
  # On Unix: Return the first argument.

  # Note (AH): Bazel `sh_binary` targets produce multiple outputs.
  # On Unix `script`, and `script.sh`.
  # On Windows `script`, `script.exe`, and `script.sh`.
  # Assuming `srcs = ["script.sh"]`.
  # This is an issue for macros like `client_server_test` which would like
  # to determine the path to executables such as.
  # `$$(canonicalize_rlocation $(rootpath {client}))`.
  # For a `sh_binary` generated `{client}` this fails since `$(rootpath )` is
  # not applicable to a target with multiple outputs and we have to use
  # `$(rootpaths )` instead. This happens to work on Unix because the first
  # item returned by `$(rootpaths )` is correctly executable. However, on
  # Windows the required `.exe` wrapper is only the second item returned by
  # `$(rootpaths )` and consequently the obtained `$client` path cannot be
  # executed on Windows.
  # See https://github.com/bazelbuild/bazel/issues/11820.
  if [[ %os% = windows ]]; then
    for arg in "$@"; do
      if [[ $arg = *.exe ]]; then
        echo "$arg"
        return
      fi
    done
    echo "$1"
  else
    echo "$1"
  fi
}
set -e
%cmd%
# vim: ft=sh
