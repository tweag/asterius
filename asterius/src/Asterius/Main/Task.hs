{-# LANGUAGE DuplicateRecordFields #-}

module Asterius.Main.Task
  ( Target (..),
    Backend (..),
    Task,
    target,
    backend,
    optimizeLevel,
    shrinkLevel,
    inputHS,
    inputEntryMJS,
    outputDirectory,
    outputBaseName,
    hasMain,
    tailCalls,
    gcSections,
    bundle,
    debug,
    outputIR,
    run,
    verboseErr,
    yolo,
    consoleHistory,
    extraGHCFlags,
    exportFunctions,
    extraRootSymbols,
    gcThreshold,
    defTask,
  )
where

import Asterius.Types (AsteriusEntitySymbol)

data Target
  = Node
  | Browser
  deriving (Eq)

data Backend
  = WasmToolkit
  | Binaryen
  deriving (Eq)

data Task
  = Task
      { target :: Target,
        backend :: Backend,
        optimizeLevel, shrinkLevel :: Int,
        inputHS :: FilePath,
        inputEntryMJS :: Maybe FilePath,
        outputDirectory :: FilePath,
        outputBaseName :: String,
        hasMain, tailCalls, gcSections, bundle, debug, outputIR, run, verboseErr, yolo, consoleHistory :: Bool,
        extraGHCFlags :: [String],
        exportFunctions, extraRootSymbols :: [AsteriusEntitySymbol],
        gcThreshold :: Int
      }

defTask :: Task
defTask = Task
  { target = Node,
    backend = Binaryen,
    optimizeLevel = 4,
    shrinkLevel = 2,
    inputHS = error "Asterius.Main.parseTask: missing inputHS",
    outputDirectory = error "Asterius.Main.parseTask: missing outputDirectory",
    outputBaseName = error "Asterius.Main.parseTask: missing outputBaseName",
    inputEntryMJS = Nothing,
    hasMain = True,
    tailCalls = False,
    gcSections = True,
    bundle = False,
    debug = False,
    outputIR = False,
    run = False,
    verboseErr = False,
    yolo = False,
    consoleHistory = False,
    extraGHCFlags = [],
    exportFunctions = [],
    extraRootSymbols = [],
    gcThreshold = 64
  }
