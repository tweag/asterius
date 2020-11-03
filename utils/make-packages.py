#!/usr/bin/env python3

import glob
import os
import re
import shutil
import subprocess

ghc_repo_url = "https://github.com/TerrorJack/ghc.git"
ghc_repo_branch = "asterius-8.8"
workdir = os.getcwd()
ghc_repo_path = os.path.join(workdir, "ghc")
hadrian_path = os.path.join(ghc_repo_path, "hadrian", "build.stack.sh")
ghci_asterius_path = os.path.join(workdir, "ghci-asterius")
ghc_asterius_path = os.path.join(workdir, "ghc-asterius")

autogen_files = [
    "_build/generated/GHCConstantsHaskellExports.hs",
    "_build/generated/GHCConstantsHaskellType.hs",
    "_build/generated/GHCConstantsHaskellWrappers.hs",
    "_build/stage0/compiler/build/Config.hs",
    "_build/stage0/compiler/build/ghc_boot_platform.h",
    "_build/stage0/compiler/build/primop-can-fail.hs-incl",
    "_build/stage0/compiler/build/primop-code-size.hs-incl",
    "_build/stage0/compiler/build/primop-commutable.hs-incl",
    "_build/stage0/compiler/build/primop-data-decl.hs-incl",
    "_build/stage0/compiler/build/primop-fixity.hs-incl",
    "_build/stage0/compiler/build/primop-has-side-effects.hs-incl",
    "_build/stage0/compiler/build/primop-list.hs-incl",
    "_build/stage0/compiler/build/primop-out-of-line.hs-incl",
    "_build/stage0/compiler/build/primop-primop-info.hs-incl",
    "_build/stage0/compiler/build/primop-strictness.hs-incl",
    "_build/stage0/compiler/build/primop-tag.hs-incl",
    "_build/stage0/compiler/build/primop-vector-tycons.hs-incl",
    "_build/stage0/compiler/build/primop-vector-tys-exports.hs-incl",
    "_build/stage0/compiler/build/primop-vector-tys.hs-incl",
    "_build/stage0/compiler/build/primop-vector-uniques.hs-incl"
]


def ghc_checkout():
    shutil.rmtree(ghc_repo_path, True)
    os.makedirs(ghc_repo_path)
    subprocess.run([
        "git",
        "clone",
        "--branch=" + ghc_repo_branch,
        "--depth=1",
        ghc_repo_url,
        ghc_repo_path,
    ],
                   check=True)
    subprocess.run([
        "git",
        "submodule",
        "update",
        "--init",
        "--recursive",
    ],
                   cwd=ghc_repo_path,
                   check=True)


def ghc_clean():
    subprocess.run(["git", "clean", "-xdf"], cwd=ghc_repo_path, check=True)
    subprocess.run(
        ["git", "submodule", "foreach", "--recursive", "git", "clean", "-xdf"],
        cwd=ghc_repo_path,
        check=True)


def ghc_configure():
    subprocess.run([os.path.join(ghc_repo_path, "boot"), "--hadrian"],
                   cwd=ghc_repo_path,
                   check=True)
    subprocess.run([
        "stack",
        "--stack-yaml=" + os.path.join(ghc_repo_path, "hadrian", "stack.yaml"),
        "exec", "--no-ghc-package-path", "--no-stack-exe", "--",
        os.path.join(ghc_repo_path, "configure")
    ],
                   cwd=ghc_repo_path,
                   check=True)


def patch_hadrian():
    with open(os.path.join(ghc_repo_path, "hadrian", "stack.yaml"),
              mode="w") as f:
        f.write("resolver: lts-16.20\n")
    shutil.copy(os.path.join(workdir, "UserSettings.hs"),
                os.path.join(ghc_repo_path, "hadrian", "UserSettings.hs"))


def make_hadrian():
    patch_hadrian()
    subprocess.run([hadrian_path, "--version"], cwd=ghc_repo_path, check=True)


def make_autogen():
    subprocess.run([hadrian_path, "-j"] + autogen_files,
                   cwd=ghc_repo_path,
                   check=True)


def patch_ghci_cabal():
    shutil.move(os.path.join(ghci_asterius_path, "ghci.cabal"),
                os.path.join(ghci_asterius_path, "ghci-asterius.cabal"))
    with open(os.path.join(ghci_asterius_path, "ghci-asterius.cabal"),
              mode="r") as h:
        ls = []
        for l in h.readlines():
            if l.strip().lower().startswith("name:"):
                ls.append("name: ghci-asterius\n")
            elif l.strip().lower().startswith("default: false"):
                ls.append("    Default: True\n")
            else:
                ls.append(l)
    with open(os.path.join(ghci_asterius_path, "ghci-asterius.cabal"),
              mode="w") as h:
        h.writelines(ls)


def patch_ghc_cabal():
    shutil.move(os.path.join(ghc_asterius_path, "ghc.cabal"),
                os.path.join(ghc_asterius_path, "ghc-asterius.cabal"))
    with open(os.path.join(ghc_asterius_path, "ghc-asterius.cabal"),
              mode="r") as h:
        ls = []
        for l in h.readlines():
            if l.strip().lower().startswith("name:"):
                ls.append("name: ghc-asterius\n")
            elif l.strip().lower().startswith("exposed:"):
                ls += [
                    "    Include-Dirs: autogen\n",
                    "    CPP-Options: -DGHCI -DSTAGE=2\n",
                    "    cc-options: -DTHREADED_RTS\n",
                    "    hs-source-dirs: autogen\n"
                ]
            elif l.strip().lower().startswith("ghci =="):
                ls.append("                   ghci-asterius == 8.8.4\n")
            elif l.strip().lower().startswith("ghc-options: -this-unit-id"):
                pass
            elif l.strip().startswith("ghci/keepCAFsForGHCi.c"):
                pass
            else:
                ls.append(l)
    with open(os.path.join(ghc_asterius_path, "ghc-asterius.cabal"),
              mode="w") as h:
        h.writelines(ls)


def patch_ghc_include():
    for f in glob.glob(os.path.join(ghc_asterius_path, "**", "*.hs"),
                       recursive=True):
        with open(f, mode="r") as h:
            s = re.sub('#include "(\.\./)+includes/', '#include "', h.read())
        with open(f, mode="w") as h:
            h.write(s)


def make_ghci_asterius():
    shutil.rmtree(ghci_asterius_path, True)
    shutil.copytree(os.path.join(ghc_repo_path, "libraries", "ghci"),
                    ghci_asterius_path)
    patch_ghci_cabal()


def make_ghc_asterius():
    shutil.rmtree(ghc_asterius_path, True)
    shutil.copytree(os.path.join(ghc_repo_path, "compiler"), ghc_asterius_path)
    autogen_path = os.path.join(ghc_asterius_path, "autogen")
    os.mkdir(autogen_path)
    shutil.copy(os.path.join(ghc_repo_path, "includes", "CodeGen.Platform.hs"),
                autogen_path)
    for f in autogen_files:
        shutil.copy(os.path.join(ghc_repo_path, f), autogen_path)
    patch_ghc_cabal()
    patch_ghc_include()


if __name__ == "__main__":
    ghc_checkout()
    ghc_clean()
    make_hadrian()
    ghc_configure()
    make_autogen()
    make_ghci_asterius()
    make_ghc_asterius()
