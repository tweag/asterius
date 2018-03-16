{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Data.Foldable
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
                for_
                  [ ("core", ppShow core)
                  , ("stg", ppShow stg)
                  , ("cmm", ppShow cmm)
                  , ("cmm-raw", ppShow cmmRaw)
                  ] $ \(e, s) -> writeFile (obj_fn $ "ddump-" ++ e ++ "-ast") s
        }
