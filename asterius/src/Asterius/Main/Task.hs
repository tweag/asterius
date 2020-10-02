{-# LANGUAGE DuplicateRecordFields #-}

module Asterius.Main.Task
  ( Target (..),
    Task,
    target,
    optimizeLevel,
    shrinkLevel,
    inputHS,
    inputEntryMJS,
    outputDirectory,
    outputBaseName,
    hasMain,
    validate,
    tailCalls,
    gcSections,
    bundle,
    debug,
    outputIR,
    run,
    verboseErr,
    pic,
    yolo,
    consoleHistory,
    extraGHCFlags,
    exportFunctions,
    extraRootSymbols,
    gcThreshold,
    defTask,
  )
where

import Asterius.Types (EntitySymbol)

data Target
  = Node
  | Browser
  deriving (Eq)

data Task
  = Task
      { target :: Target,
        optimizeLevel, shrinkLevel :: Int,
        inputHS :: FilePath,
        inputEntryMJS :: Maybe FilePath,
        outputDirectory :: FilePath,
        outputBaseName :: String,
        hasMain, validate, tailCalls, gcSections, bundle, debug, outputIR, run, verboseErr, pic, yolo, consoleHistory :: Bool,
        extraGHCFlags :: [String],
        exportFunctions, extraRootSymbols :: [EntitySymbol],
        gcThreshold :: Int
      }

defTask :: Task
defTask = Task
  { target = Node,
    optimizeLevel = 0,
    shrinkLevel = 0,
    inputHS = error "Asterius.Main.parseTask: missing inputHS",
    outputDirectory = error "Asterius.Main.parseTask: missing outputDirectory",
    outputBaseName = error "Asterius.Main.parseTask: missing outputBaseName",
    inputEntryMJS = Nothing,
    hasMain = True,
    validate = True,
    tailCalls = False,
    gcSections = True,
    bundle = False,
    debug = False,
    outputIR = False,
    run = False,
    verboseErr = False,
    pic = False,
    yolo = False,
    consoleHistory = False,
    extraGHCFlags = [],
    exportFunctions = [],
    extraRootSymbols = [],
    gcThreshold = 64
  }
