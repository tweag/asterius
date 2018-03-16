{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Data.List.Extra
import GHC
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import Language.Haskell.GHC.Toolkit.IROrphans ()
import System.Directory
import System.FilePath
import Text.Show.Pretty
import UnliftIO.Environment

frontendPlugin :: FrontendPlugin
frontendPlugin =
  frontendPluginFromCompiler $ do
    obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
    pure $
      defaultCompiler
        { withIR =
            \ModSummary {ms_mod = Module {..}} IR {..} ->
              liftIO $ do
                let obj_fn ext =
                      obj_topdir </> unitIdString moduleUnitId </>
                      foldr1
                        (</>)
                        (wordsBy (== '.') (moduleNameString moduleName)) <.>
                      ext
                createDirectoryIfMissing True $ takeDirectory $ obj_fn ""
                writeFile (obj_fn "ddump-core-ast") $ ppShow core
                writeFile (obj_fn "ddump-stg-ast") $ ppShow stg
                writeFile (obj_fn "ddump-cmm-raw-ast") $ ppShow cmmRaw
        }
