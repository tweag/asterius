module Asterius.BuildInfo
  ( ghc
  , ghcPkg
  , ghcLibDir
  , ahc
  , ahcAr
  , dataDir
  , packageDBStack
  ) where

import BuildInfo_asterius
import System.Directory
import System.FilePath

ahc, ahcAr :: FilePath
ahc = binDir </> "ahc" <.> exeExtension

ahcAr = binDir </> "ahc-ar" <.> exeExtension
