module Asterius.BuildInfo
  ( ahc,
    ahcPkg,
    ahcLd,
    ahcDist,
    unlit,
    dataDir,
  )
where

import qualified Paths_asterius
import System.Directory
import System.FilePath
import System.IO.Unsafe

{-# NOINLINE binDir #-}
binDir :: FilePath
binDir = unsafePerformIO Paths_asterius.getBinDir

{-# NOINLINE dataDir #-}
dataDir :: FilePath
dataDir = unsafePerformIO Paths_asterius.getDataDir

ahc, ahcPkg, ahcLd, ahcDist, unlit :: FilePath
ahc = binDir </> "ahc" <.> exeExtension
ahcPkg = binDir </> "ahc-pkg" <.> exeExtension
ahcLd = binDir </> "ahc-ld" <.> exeExtension
ahcDist = binDir </> "ahc-dist" <.> exeExtension
unlit = binDir </> "unlit" <.> exeExtension
