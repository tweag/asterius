{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Asterius.CodeGen
import Cmm
import qualified Data.Map.Strict as M
import Data.Traversable
import DynFlags
import Language.Haskell.GHC.Toolkit.BuildInfo
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Run
import Language.WebAssembly.NIR
import System.Directory
import System.FilePath
import Text.Show.Pretty

main :: IO ()
main = do
  let rts_path = bootLibsPath </> "rts"
  cmm_fns <-
    map (rts_path </>) . filter ((== ".cmm") . takeExtension) <$>
    listDirectory rts_path
  rawcmms <-
    map (\(k, CmmIR {..}) -> (k, cmmRaw)) . M.toList <$>
    runCmm defaultConfig cmm_fns
  ams <-
    for rawcmms $ \(k, rawcmm_list) ->
      fmap (k, ) $
      for rawcmm_list $ \rawcmm ->
        ( marshalCLabel unsafeGlobalDynFlags $
          case rawcmm of
            CmmData _ (Statics clbl _) -> clbl
            CmmProc _ clbl _ _ -> clbl
        ,) .
        (\decl -> (decl, collectUnresolvedLabels decl)) <$>
        marshalCmmDecl unsafeGlobalDynFlags rawcmm
  pPrint ams
