{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
    AsteriusRepModule,
    Module,
  )
import Data.String
import Language.JavaScript.Inline.Core
import System.FilePath

linkNonMain :: AsteriusRepModule -> [EntitySymbol] -> IO (Module, LinkReport)
linkNonMain module_rep extra_syms = do
  (_, m, link_report) <-
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
      module_rep
  return (m, link_report)

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
        validate = False,
        tailCalls = True,
        yolo = True,
        Asterius.Main.Task.hasMain = False,
        Asterius.Main.Task.verboseErr = True,
        extraRootSymbols = extra_syms
      }

newAsteriusInstanceNonMain ::
  Session ->
  FilePath ->
  [EntitySymbol] ->
  AsteriusRepModule ->
  IO JSVal
newAsteriusInstanceNonMain s p extra_syms module_rep = do
  linkNonMain module_rep extra_syms >>= distNonMain p extra_syms
  let rts_path = dataDir </> "rts" </> "rts.mjs"
      req_path = p -<.> "req.mjs"
      wasm_path = p -<.> "wasm"
  rts_val <- importMJS s rts_path
  req_mod_val <- importMJS s req_path
  req_val <- eval @JSVal s $ toJS req_mod_val <> ".default"
  mod_val <-
    eval @JSVal s $
      "import('fs').then(fs => fs.promises.readFile("
        <> fromString (show wasm_path)
        <> ")).then(buf => WebAssembly.compile(buf))"
  eval s $
    toJS rts_val
      <> ".newAsteriusInstance(Object.assign("
      <> toJS req_val
      <> ",{module:"
      <> toJS mod_val
      <> "}))"
