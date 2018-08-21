module Language.Haskell.GHC.Toolkit.BuildInfo
  ( bootLibsPath
  , ghcLibDir
  , dataDir
  , gccPath
  , ahcGccPath
  ) where

import BuildInfo_ghc_toolkit
import System.Directory
import System.FilePath

bootLibsPath, ahcGccPath :: FilePath
bootLibsPath = dataDir </> "boot-libs"

ahcGccPath = binDir </> "ahc-gcc" <.> exeExtension
