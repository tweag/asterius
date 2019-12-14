{-# LANGUAGE DuplicateRecordFields #-}

module Asterius.Main.Task
  ( Target (..),
    Task,
    target,
    inputHS,
    inputEntryMJS,
    outputDirectory,
    outputBaseName,
    tailCalls,
    gcSections,
    fullSymTable,
    bundle,
    binaryen,
    debug,
    outputLinkReport,
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

data Task
  = Task
      { target :: Target,
        inputHS :: FilePath,
        inputEntryMJS :: Maybe FilePath,
        outputDirectory :: FilePath,
        outputBaseName :: String,
        tailCalls, gcSections, fullSymTable, bundle, binaryen, debug, outputLinkReport, outputIR, run, verboseErr, yolo :: Bool,
        extraGHCFlags :: [String],
        exportFunctions, extraRootSymbols :: [AsteriusEntitySymbol],
        gcThreshold :: Int
      }

defTask :: Task
defTask = Task
  { target = Node,
    inputHS = error "Asterius.Main.parseTask: missing inputHS",
    outputDirectory = error "Asterius.Main.parseTask: missing outputDirectory",
    outputBaseName = error "Asterius.Main.parseTask: missing outputBaseName",
    inputEntryMJS = Nothing,
    tailCalls = False,
    gcSections = True,
    fullSymTable = False,
    bundle = False,
    binaryen = False,
    debug = False,
    outputLinkReport = False,
    outputIR = False,
    run = False,
    verboseErr = False,
    yolo = False,
    extraGHCFlags = [],
    exportFunctions = [],
    extraRootSymbols = [],
    gcThreshold = 64
  }
