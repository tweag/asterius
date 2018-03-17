module Language.Haskell.GHC.Toolkit.BuildInfo
  ( bootLibsPath
  , ghcLibDir
  ) where

import BuildInfo_ghc_toolkit
import System.FilePath

bootLibsPath :: FilePath
bootLibsPath = dataDir </> "boot-libs"
