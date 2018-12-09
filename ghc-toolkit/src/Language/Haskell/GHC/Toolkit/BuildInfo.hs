module Language.Haskell.GHC.Toolkit.BuildInfo
  ( bootLibsPath
  , sandboxGhcLibDir
  , ghcLibDir
  , dataDir
  , gccPath
  ) where

import BuildInfo_ghc_toolkit
import System.FilePath

bootLibsPath, sandboxGhcLibDir :: FilePath
bootLibsPath = dataDir </> "boot-libs"

sandboxGhcLibDir = dataDir </> "ghc-libdir"
