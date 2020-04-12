{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSRun.NonMain
  ( distNonMain,
    newAsteriusInstanceNonMain,
  )
where

import Asterius.BuildInfo
import Asterius.Ld
  ( LinkTask (..),
    linkModules,
  )
import Asterius.Main (ahcDistMain)
import Asterius.Main.Task
import Asterius.Resolve
import Asterius.Types
  ( EntitySymbol,
    AsteriusModule,
    Module,
  )
import Data.String
import Language.JavaScript.Inline.Core
import System.FilePath

linkNonMain :: AsteriusModule -> [EntitySymbol] -> (Module, LinkReport)
linkNonMain store_m extra_syms = (m, link_report)
  where
    (_, m, link_report) =
      linkModules
        LinkTask
          { progName = "",
            linkOutput = "",
            linkObjs = [],
            linkLibs = [],
            linkModule = mempty,
            Asterius.Ld.hasMain = False,
            Asterius.Ld.debug = False,
            Asterius.Ld.gcSections = True,
            Asterius.Ld.verboseErr = True,
            Asterius.Ld.outputIR = Nothing,
            rootSymbols = extra_syms,
            Asterius.Ld.exportFunctions = []
          }
        store_m

distNonMain ::
  FilePath -> [EntitySymbol] -> (Module, LinkReport) -> IO ()
distNonMain p extra_syms =
  ahcDistMain
    putStrLn
    defTask
      { optimizeLevel = 0,
        shrinkLevel = 0,
        inputHS = p,
        outputDirectory = takeDirectory p,
        outputBaseName = takeBaseName p,
        tailCalls = True,
        Asterius.Main.Task.hasMain = False,
        Asterius.Main.Task.verboseErr = True,
        extraRootSymbols = extra_syms
      }

newAsteriusInstanceNonMain ::
  JSSession ->
  FilePath ->
  [EntitySymbol] ->
  AsteriusModule ->
  IO JSVal
newAsteriusInstanceNonMain s p extra_syms m = do
  distNonMain p extra_syms $ linkNonMain m extra_syms
  let rts_path = dataDir </> "rts" </> "rts.mjs"
      req_path = p -<.> "req.mjs"
      wasm_path = p -<.> "wasm"
  rts_val <- importMJS s rts_path
  req_mod_val <- importMJS s req_path
  req_val <- eval s $ takeJSVal req_mod_val <> ".default"
  mod_val <-
    eval s $
      "import('fs').then(fs => fs.promises.readFile("
        <> fromString (show wasm_path)
        <> ")).then(buf => WebAssembly.compile(buf))"
  eval s $
    takeJSVal rts_val
      <> ".newAsteriusInstance(Object.assign("
      <> takeJSVal req_val
      <> ",{module:"
      <> takeJSVal mod_val
      <> "}))"
