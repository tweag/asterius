-----------------------------------------------------------------------------
-- |
-- Module      :  Asterius.BuildInfo
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Paths for data and binary files.
--
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Asterius.BuildInfo
  ( ahc,
    ahcPkg,
    ahcLd,
    ahcDist,
    setupGhcPrim,
    unlit,
    dataDir,
    getAhc,
    getAhcPkg,
    getAhcLd,
    getAhcDist,
    Paths_asterius.getDataDir,
    getBootDir,
    rootBootDir,
    asteriusSandboxGhcLibDir,
    asteriusBootLibsPath
  )
where

import qualified Paths_asterius
import Control.Exception (IOException, catch)
import System.Directory
import System.Environment (getEnv)
import System.FilePath
import System.IO.Unsafe
import Language.Haskell.GHC.Toolkit.BuildInfo (bootLibsPath, sandboxGhcLibDir)

{-# NOINLINE binDir #-}
binDir :: FilePath
binDir = unsafePerformIO Paths_asterius.getBinDir

{-# NOINLINE dataDir #-}
dataDir :: FilePath
dataDir = unsafePerformIO Paths_asterius.getDataDir

ahc :: FilePath
ahc = binDir </> "ahc" <.> exeExtension

getBin :: FilePath -> IO FilePath
getBin a = (\binDir -> binDir </> a <.> exeExtension) <$> Paths_asterius.getBinDir

ahcPkg :: FilePath
ahcPkg = binDir </> "ahc-pkg" <.> exeExtension

getAhc, getAhcPkg, getAhcLd, getAhcDist, getBootDir :: IO FilePath
getAhc = getBin "ahc"

ahcLd :: FilePath
ahcLd = binDir </> "ahc-ld" <.> exeExtension

getAhcPkg = getBin "ahc-pkg"

ahcDist :: FilePath
ahcDist = binDir </> "ahc-dist" <.> exeExtension

getAhcLd = getBin "ahc-ld"

getAhcDist = getBin "ahc-dist"

getBootDir = catch (getEnv "asterius_bootdir")
  (\(_ :: IOException) -> Paths_asterius.getDataDir)

-- Allow nix to override the dir used for ".boot"
-- this is called rootBootDir as bootDir is used in the code
-- to refer to the ".boot" dir itself
rootBootDir :: FilePath
rootBootDir = unsafePerformIO (catch (getEnv "asterius_bootdir")
  (\(_ :: IOException) -> Paths_asterius.getDataDir))

-- Allow nix to override the sandboxGhcLibDir (from ghc toolkit)
asteriusSandboxGhcLibDir :: FilePath
asteriusSandboxGhcLibDir = unsafePerformIO (catch (getEnv "sandbox_ghc_lib_dir")
  (\(_ :: IOException) -> pure sandboxGhcLibDir))

-- Allow nix to override the bootLibsPath (from ghc toolkit)
asteriusBootLibsPath :: FilePath
asteriusBootLibsPath = unsafePerformIO (catch ((</> "libraries") <$> getEnv "boot_libs_path")
  (\(_ :: IOException) -> pure bootLibsPath))

setupGhcPrim :: FilePath
setupGhcPrim = binDir </> "Setup-ghc-prim" <.> exeExtension

unlit :: FilePath
unlit = binDir </> "unlit" <.> exeExtension
