module Asterius.BuildInfo
  ( ghc
  , ghcPkg
  , ghcLibDir
  , ahc
  , ahcPkg
  , ahcAr
  , ahcLd
  , ahcDist
  , dataDir
  , packageDBStack
  ) where

import BuildInfo_asterius
import System.Directory
import System.FilePath

ahc, ahcPkg, ahcAr, ahcLd, ahcDist :: FilePath
ahc = binDir </> "ahc" <.> exeExtension

ahcPkg = binDir </> "ahc-pkg" <.> exeExtension

ahcAr = binDir </> "ahc-ar" <.> exeExtension

ahcLd = binDir </> "ahc-ld" <.> exeExtension

ahcDist = binDir </> "ahc-dist" <.> exeExtension
