-----------------------------------------------------------------------------
-- |
-- Module      :  Asterius.BuildInfo
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Paths for data and binary files.
--
-----------------------------------------------------------------------------

module Asterius.BuildInfo
  ( ahc,
    ahcPkg,
    ahcLd,
    ahcDist,
    setupGhcPrim,
    unlit,
    dataDir,
    rootBootDir,
  )
where

import qualified Paths_asterius
import System.Directory
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

ahcPkg :: FilePath
ahcPkg = binDir </> "ahc-pkg" <.> exeExtension

ahcLd :: FilePath
ahcLd = binDir </> "ahc-ld" <.> exeExtension

ahcDist :: FilePath
ahcDist = binDir </> "ahc-dist" <.> exeExtension

-- Allow nix to override the dir used for ".boot"
-- this is called rootBootDir as bootDir is used in the code
-- to refer to the ".boot" dir itself
rootBootDir = unsafePerformIO (catch (getEnv "asterius_bootdir")
  (\(_ :: IOException) -> Paths_asterius.getDataDir))

-- Allow nix to override the sandboxGhcLibDir (from ghc toolkit)
asteriusSandboxGhcLibDir = unsafePerformIO (catch (getEnv "sandbox_ghc_lib_dir")
  (\(_ :: IOException) -> pure sandboxGhcLibDir))

-- Allow nix to override the bootLibsPath (from ghc toolkit)
asteriusBootLibsPath = unsafePerformIO (catch ((</> "libraries") <$> getEnv "boot_libs_path")
  (\(_ :: IOException) -> pure bootLibsPath))

setupGhcPrim :: FilePath
setupGhcPrim = binDir </> "Setup-ghc-prim" <.> exeExtension

unlit :: FilePath
unlit = binDir </> "unlit" <.> exeExtension
