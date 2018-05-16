{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Asterius.Boot
import Asterius.CodeGen
import Asterius.Internals
import Asterius.Marshal
import Asterius.Resolve
import Asterius.Store
import Bindings.Binaryen.Raw
import Control.Exception
import qualified Data.Map as M
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Run
import Prelude hiding (IO)
import System.Directory
import System.FilePath
import System.IO hiding (IO)
import Text.Show.Pretty

main :: IO ()
main = do
  boot_args <- getDefaultBootArgs
  let obj_topdir = bootDir boot_args </> "asterius_lib"
  pwd <- getCurrentDirectory
  let test_path = pwd </> "test" </> "fact-dump"
  withCurrentDirectory test_path $ do
    putStrLn "Compiling Fact.."
    [(ms_mod, ir)] <- M.toList <$> runHaskell defaultConfig ["Fact.hs"]
    case runCodeGen (marshalHaskellIR ir) GHC.unsafeGlobalDynFlags ms_mod of
      Left err -> throwIO err
      Right m -> do
        putStrLn "Dumping IR of Fact.."
        writeFile "Fact.txt" $ ppShow m
        putStrLn "Chasing Fact_root_closure.."
        store' <- decodeFile (obj_topdir </> "asterius_store")
        let store = addModule (marshalToModuleSymbol ms_mod) m store'
            (maybe_final_m, report) =
              linkStart
                store
                [ "Fact_root_closure"
                , "StgRun"
                , "stg_stop_thread_info"
                , "allocGroup"
                , "rts_evalIO"
                , "newCAF"
                , "rts_lock"
                ]
        writeDot "Fact.gv" report
        let Just final_m = maybe_final_m
        pPrint final_m
        hFlush stdout
        m_ref <- marshalModule final_m
        print m_ref
        c_BinaryenModulePrint m_ref
        c_BinaryenModuleValidate m_ref >>= print
        c_BinaryenModuleDispose m_ref
