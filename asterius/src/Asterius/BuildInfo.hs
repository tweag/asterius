module Asterius.BuildInfo
  ( ghc
  , ghcPkg
  , mkdir
  , cp
  , node
  , sh
  , ghcLibDir
  , ahc
  , dataDir
  , packageDBStack
  ) where

import BuildInfo_asterius
import System.Directory
import System.FilePath

ahc :: FilePath
ahc = binDir </> "ahc" <.> exeExtension
