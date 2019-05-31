{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.GenericPackageDescription
import System.Directory
import System.Process

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook =
          \t@(g_pkg_descr, _) c -> do
            lbi <- confHook simpleUserHooks t c
            let pkg_descr = packageDescription g_pkg_descr
                npm_utils_installdirs =
                  absoluteInstallDirs pkg_descr lbi NoCopyDest
                npm_utils_datadir = datadir npm_utils_installdirs
            createDirectoryIfMissing True npm_utils_datadir
            withCurrentDirectory npm_utils_datadir $
              callCommand "npm install parcel-bundler"
            pure lbi
      }
