{-# LANGUAGE DuplicateRecordFields #-}

module Asterius.Main.Task
  ( Target (..),
    Backend (..),
    Task,
    target,
    backend,
    inputHS,
    inputEntryMJS,
    outputDirectory,
    outputBaseName,
    tailCalls,
    gcSections,
    bundle,
    debug,
    outputIR,
    run,
    verboseErr,
    yolo,
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
        inputHS :: FilePath,
        inputEntryMJS :: Maybe FilePath,
        outputDirectory :: FilePath,
        outputBaseName :: String,
        tailCalls, gcSections, bundle, debug, outputIR, run, verboseErr, yolo :: Bool,
        extraGHCFlags :: [String],
        exportFunctions, extraRootSymbols :: [AsteriusEntitySymbol],
        gcThreshold :: Int
      }

defTask :: Task
defTask = Task
  { target = Node,
    backend = WasmToolkit,
    inputHS = error "Asterius.Main.parseTask: missing inputHS",
    outputDirectory = error "Asterius.Main.parseTask: missing outputDirectory",
    outputBaseName = error "Asterius.Main.parseTask: missing outputBaseName",
    inputEntryMJS = Nothing,
    tailCalls = False,
    gcSections = True,
    bundle = False,
    debug = False,
    outputIR = False,
    run = False,
    verboseErr = False,
    yolo = False,
    extraGHCFlags = [],
    exportFunctions = [],
    extraRootSymbols = [],
    gcThreshold = 64
  }
