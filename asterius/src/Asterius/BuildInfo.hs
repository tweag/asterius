module Asterius.BuildInfo
  ( ghc,
    ghcPkg,
    ghcLibDir,
    ahc,
    ahcPkg,
    ahcLd,
    ahcDist,
    dataDir,
  )
where

import BuildInfo_asterius
import System.Directory
import System.FilePath

ahc, ahcPkg, ahcLd, ahcDist :: FilePath
ahc = binDir </> "ahc" <.> exeExtension
ahcPkg = binDir </> "ahc-pkg" <.> exeExtension
ahcLd = binDir </> "ahc-ld" <.> exeExtension
ahcDist = binDir </> "ahc-dist" <.> exeExtension
