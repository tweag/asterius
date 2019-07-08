{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSRun.NonMain
  ( newAsteriusInstanceNonMain
  ) where

import Asterius.Ld (LinkTask(..), linkModules)
import Asterius.Main (Target(..), Task(..), ahcDistMain)
import Asterius.Resolve
import Asterius.Types (AsteriusEntitySymbol, AsteriusModule, Module)
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core
import System.FilePath

linkNonMain :: AsteriusModule -> [AsteriusEntitySymbol] -> (Module, LinkReport)
linkNonMain store_m extra_syms = (m, link_report)
  where
    (_, m, link_report) =
      linkModules
        LinkTask
          { progName = ""
          , linkOutput = ""
          , linkObjs = []
          , linkLibs = []
          , Asterius.Ld.debug = False
          , Asterius.Ld.gcSections = True
          , Asterius.Ld.binaryen = False
          , Asterius.Ld.verboseErr = True
          , Asterius.Ld.outputIR = Nothing
          , rootSymbols = extra_syms
          , Asterius.Ld.exportFunctions = []
          }
        store_m

distNonMain ::
     FilePath -> [AsteriusEntitySymbol] -> (Module, LinkReport) -> IO ()
distNonMain p extra_syms =
  ahcDistMain
    (\_ -> pure ())
    Task
      { target = Node
      , inputHS = p
      , outputDirectory = takeDirectory p
      , outputBaseName = takeBaseName p
      , inputEntryMJS = Nothing
      , tailCalls = False
      , Asterius.Main.gcSections = True
      , fullSymTable = True
      , bundle = False
      , Asterius.Main.binaryen = False
      , Asterius.Main.debug = False
      , outputLinkReport = False
      , Asterius.Main.outputIR = False
      , run = False
      , Asterius.Main.verboseErr = True
      , extraGHCFlags = ["-no-hs-main"]
      , Asterius.Main.exportFunctions = []
      , extraRootSymbols = extra_syms
      }

newAsteriusInstanceNonMain ::
     JSSession
  -> FilePath
  -> [AsteriusEntitySymbol]
  -> AsteriusModule
  -> IO JSVal
newAsteriusInstanceNonMain s p extra_syms m = do
  distNonMain p extra_syms $ linkNonMain m extra_syms
  let lib_path = p -<.> "lib.mjs"
  mod_buf <- LBS.readFile $ p -<.> "wasm"
  lib_val <- importMJS s lib_path
  f_val <- eval s $ takeJSVal lib_val <> ".default"
  buf_val <- alloc s mod_buf
  mod_val <- eval s $ "WebAssembly.compile(" <> takeJSVal buf_val <> ")"
  eval s $ takeJSVal f_val <> "(" <> takeJSVal mod_val <> ")"
