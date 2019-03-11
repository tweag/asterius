{-# LANGUAGE RecordWildCards #-}

module Asterius.Linker.Packages
  ( packageArchives
  ) where

import Data.Foldable
import Data.List
import qualified HscTypes as GHC
import qualified Packages as GHC
import System.Directory

packageArchives :: GHC.HscEnv -> IO [FilePath]
packageArchives GHC.HscEnv {..} = do
  let home_mod_infos = GHC.eltsHpt hsc_HPT
      pkg_deps =
        concatMap
          (map fst . GHC.dep_pkgs . GHC.mi_deps . GHC.hm_iface)
          home_mod_infos
  pkg_cfgs <- GHC.getPreloadPackagesAnd hsc_dflags pkg_deps
  foldrM
    (\pkg_cfg acc ->
       foldrM
         (\lib acc' ->
            (<> acc') <$>
            findFiles
              (nub . filter (not . null) . GHC.libraryDirs $ pkg_cfg)
              ("lib" <> lib <> ".a"))
         acc
         (GHC.packageHsLibs hsc_dflags pkg_cfg))
    []
    pkg_cfgs
