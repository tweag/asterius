{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as M
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.BuildInfo
import Distribution.Types.Library
import Distribution.Types.PackageDescription
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook =
          \t f -> do
            lbi@LocalBuildInfo { localPkgDescr = pkg_descr@PackageDescription {library = Just lib@Library {libBuildInfo = bi}}
                               , compiler = Compiler {compilerProperties = m}
                               } <- confHook simpleUserHooks t f
            let [clbi@LibComponentLocalBuildInfo {componentUnitId = uid}] =
                  componentNameMap lbi M.! CLibName
                amp = autogenComponentModulesDir lbi clbi
            createDirectoryIfMissing True amp
            writeFile (amp </> "BuildInfo_ghc_toolkit.hs") $
              "module BuildInfo_ghc_toolkit where\nghcLibDir :: FilePath\nghcLibDir = " ++
              show (m M.! "LibDir") ++
              "\ndataDir :: FilePath\ndataDir = " ++
              show
                (datadir $
                 absoluteComponentInstallDirs pkg_descr lbi uid NoCopyDest)
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
                                        "BuildInfo_ghc_toolkit" :
                                        otherModules bi
                                    , autogenModules =
                                        "BuildInfo_ghc_toolkit" :
                                        autogenModules bi
                                    }
                              }
                      }
                }
      }
