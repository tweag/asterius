module Language.Haskell.GHC.Toolkit.BuildInfo
  ( getBootLibsPath
  , getSandboxGhcLibDir
  , getGhcLibDir
  , getDataDir
  , getGccPath
  ) where

import BuildInfo_ghc_toolkit
import System.FilePath

getBootLibsPath, getSandboxGhcLibDir :: IO FilePath
getBootLibsPath = (</> "boot-libs") <$> getDataDir
getSandboxGhcLibDir = (</> "ghc-libdir") <$> getDataDir
