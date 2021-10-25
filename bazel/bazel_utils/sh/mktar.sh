#!/usr/bin/env bash
# Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Copy-pasted from the Bazel Bash runfiles library v2.
set -uo pipefail; f=bazel_tools/tools/bash/runfiles/runfiles.bash
source "${RUNFILES_DIR:-/dev/null}/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "${RUNFILES_MANIFEST_FILE:-/dev/null}" | cut -f2- -d' ')" 2>/dev/null || \
  source "$0.runfiles/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.exe.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  { echo>&2 "ERROR: cannot find $f"; exit 1; }; f=; set -e
# --- end runfiles.bash initialization v2 ---

set -euo pipefail
usage() {
  cat >&2 <<'EOF'
usage: mktar OUTPUT ARGS...

Creates an uncompressed tarball in OUTPUT passing ARGS to tar. The created
tarball is reproducible, i.e. it does not contain any timestamps or similar
non-deterministic inputs. See https://reproducible-builds.org/docs/archives/
EOF
}
trap usage ERR

case "$(uname -s)" in
  Darwin|Linux)
    tar=$(rlocation tar_dev_env/tar)
    ;;
  CYGWIN*|MINGW*|MSYS*)
    tar=$(rlocation tar_dev_env/usr/bin/tar.exe)
    ;;
esac

$tar cf "$1" "${@:2}" \
  --owner="0" \
  --group="0" \
  --numeric-owner \
  --mtime="2000-01-01 00:00Z" \
  --no-acls \
  --no-xattrs \
  --no-selinux \
  --sort="name" \
  --format=ustar
