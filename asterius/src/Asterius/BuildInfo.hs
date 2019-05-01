{-# LANGUAGE ScopedTypeVariables #-}
module Asterius.BuildInfo
  ( ghc
  , ghcPkg
  , ghcLibDir
  , getAhc
  , getAhcPkg
  , getAhcLd
  , getAhcDist
  , getDataDir
  , getBootDir
  ) where

import Control.Exception (catch, IOException)
import System.Environment (getEnv)
import BuildInfo_asterius
import Paths_asterius
import System.Directory
import System.FilePath

getBin :: FilePath -> IO FilePath
getBin a = (\binDir -> binDir </> a <.> exeExtension) <$> getBinDir

getAhc, getAhcPkg, getAhcLd, getAhcDist, getBootDir :: IO FilePath
getAhc = getBin "ahc"

getAhcPkg = getBin "ahc-pkg"

getAhcLd = getBin "ahc-ld"

getAhcDist = getBin "ahc-dist"

getBootDir = catch (getEnv "asterius_bootdir")
  (\(_ :: IOException) -> getDataDir)
