{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as M
import Distribution.Simple
import Distribution.Simple.BuildPaths hiding
  ( exeExtension,
  )
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.BuildInfo
import Distribution.Types.Library
import Distribution.Types.LocalBuildInfo
import Distribution.Types.PackageDescription
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook = \t f -> do
          lbi@LocalBuildInfo {localPkgDescr = pkg_descr@PackageDescription {library = Just lib@Library {libBuildInfo = bi}}, compiler = Compiler {compilerProperties = m}} <-
            confHook simpleUserHooks t f
          let [clbi@LibComponentLocalBuildInfo {componentUnitId = uid}] =
                componentNameCLBIs lbi mainLibName
              amp = autogenComponentModulesDir lbi clbi
              self_installdirs =
                absoluteComponentInstallDirs pkg_descr lbi uid NoCopyDest
              self_bindir = bindir self_installdirs
              self_datadir = datadir self_installdirs
          createDirectoryIfMissing True amp
          writeFile (amp </> "BuildInfo_ghc_toolkit.hs") $
            "module BuildInfo_ghc_toolkit where\nghcLibDir :: FilePath\nghcLibDir = "
              ++ show (m M.! "LibDir")
              ++ "\ndataDir :: FilePath\ndataDir = "
              ++ show self_datadir
              ++ "\nbinDir :: FilePath\nbinDir = "
              ++ show self_bindir
          pure
            lbi
              { localPkgDescr =
                  pkg_descr
                    { library =
                        Just
                          lib
                            { libBuildInfo =
                                bi
                                  { otherModules =
                                      "BuildInfo_ghc_toolkit"
                                        : otherModules bi,
                                    autogenModules =
                                      "BuildInfo_ghc_toolkit"
                                        : autogenModules bi
                                  }
                            }
                    }
              }
      }

mainLibName :: ComponentName
mainLibName = CLibName defaultLibName
