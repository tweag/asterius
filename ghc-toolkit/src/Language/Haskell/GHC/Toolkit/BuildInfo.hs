module Language.Haskell.GHC.Toolkit.BuildInfo
  ( bootLibsPath,
    sandboxGhcLibDir,
    dataDir,
  )
where

import Paths_ghc_toolkit
import System.FilePath
import System.IO.Unsafe

bootLibsPath :: FilePath
bootLibsPath = dataDir </> "boot-libs"

sandboxGhcLibDir :: FilePath
sandboxGhcLibDir = dataDir </> "ghc-libdir"

{-# NOINLINE dataDir #-}
dataDir :: FilePath
dataDir = unsafePerformIO getDataDir
