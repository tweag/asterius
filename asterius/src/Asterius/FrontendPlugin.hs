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
  frontendPluginFromCompiler $
  pure $
  Compiler $ \ModSummary {ms_mod = Module {..}} IR {..} ->
    liftIO $ do
      obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
      let obj_fn =
            obj_topdir </> unitIdString moduleUnitId </>
            foldr1 (</>) (wordsBy (== '.') (moduleNameString moduleName)) <.>
            "ddump-stg-ast"
      createDirectoryIfMissing True $ takeDirectory obj_fn
      writeFile obj_fn $ ppShow stg
