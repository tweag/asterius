{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Asterius.BuildInfo
  ( ghc
  , ghcPkg
  , mkdir
  , cp
  , node
  , sed
  , sh
  , ghcLibDir
  , ahc
  , dataDir
  , pkgDbStack
  ) where

import Data.Binary
import Data.Map
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Language.Haskell.TH.Syntax
import Paths_asterius
import System.Directory
import System.FilePath

ghc :: FilePath
ghc =
  $(do lbi <- runIO $ decodeFile ".lbi.buildinfo"
       let Just prog = lookupProgram ghcProgram $ withPrograms lbi
       liftString $ locationPath $ programLocation prog)

ghcPkg :: FilePath
ghcPkg =
  $(do lbi <- runIO $ decodeFile ".lbi.buildinfo"
       let Just prog = lookupProgram ghcPkgProgram $ withPrograms lbi
       liftString $ locationPath $ programLocation prog)

mkdir :: FilePath
mkdir =
  $(do lbi <- runIO $ decodeFile ".lbi.buildinfo"
       let Just prog = lookupProgram (simpleProgram "mkdir") $ withPrograms lbi
       liftString $ locationPath $ programLocation prog)

cp :: FilePath
cp =
  $(do lbi <- runIO $ decodeFile ".lbi.buildinfo"
       let Just prog = lookupProgram (simpleProgram "cp") $ withPrograms lbi
       liftString $ locationPath $ programLocation prog)

node :: FilePath
node =
  $(do lbi <- runIO $ decodeFile ".lbi.buildinfo"
       let Just prog = lookupProgram (simpleProgram "node") $ withPrograms lbi
       liftString $ locationPath $ programLocation prog)

sed :: FilePath
sed =
  $(do lbi <- runIO $ decodeFile ".lbi.buildinfo"
       let Just prog = lookupProgram (simpleProgram "sed") $ withPrograms lbi
       liftString $ locationPath $ programLocation prog)

sh :: FilePath
sh =
  $(do lbi <- runIO $ decodeFile ".lbi.buildinfo"
       let Just prog = lookupProgram (simpleProgram "sh") $ withPrograms lbi
       liftString $ locationPath $ programLocation prog)

ghcLibDir :: FilePath
ghcLibDir =
  $(do lbi <- runIO $ decodeFile ".lbi.buildinfo"
       liftString $ compilerProperties (compiler lbi) ! "LibDir")

ahc :: FilePath
ahc =
  $(do p <- runIO getBinDir
       liftString $ p </> "ahc" <.> exeExtension)

dataDir :: FilePath
dataDir =
  $(do p <- runIO getDataDir
       liftString p)

pkgDbStack :: PackageDBStack
pkgDbStack =
  $(do lbi <- runIO $ decodeFile ".lbi.buildinfo"
       pure $
         ListE
           [ case pkgdb of
             GlobalPackageDB -> ConE 'GlobalPackageDB
             UserPackageDB -> ConE 'UserPackageDB
             SpecificPackageDB p ->
               AppE (ConE 'SpecificPackageDB) (LitE $ StringL p)
           | pkgdb <- withPackageDB lbi
           ])
