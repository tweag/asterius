{-# LANGUAGE OverloadedStrings #-}

import Asterius.Boot
import Asterius.CodeGen
import Asterius.SymbolDB
import Asterius.Types
import Control.Exception
import qualified Data.Map as M
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Run
import System.Directory
import System.FilePath
import Text.Show.Pretty

main :: IO ()
main = do
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
        cache <- initAsteriusModuleCache obj_topdir
        chase
          cache
          AsteriusEntitySymbol
            {entityKind = StaticsEntity, entityName = "Fact_root_closure"} >>=
          print
  where
    obj_topdir = bootDir defaultBootArgs </> "asterius_lib"
