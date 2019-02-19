module Asterius.BuildInfo
  ( ghc
  , ghcPkg
  , ghcLibDir
  , ahc
  , ahcPkg
  , ahcAr
  , dataDir
  , packageDBStack
  ) where

import BuildInfo_asterius
import System.Directory
import System.FilePath

ahc, ahcPkg, ahcAr :: FilePath
ahc = binDir </> "ahc" <.> exeExtension

ahcPkg = binDir </> "ahc-pkg" <.> exeExtension

ahcAr = binDir </> "ahc-ar" <.> exeExtension
