{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Asterius.JSRun.NonMain
  ( distNonMain,
  )
where

import Asterius.Main (ahcDistMain)
import Asterius.Main.Task
import Asterius.Resolve
import Asterius.Types
  ( EntitySymbol,
    Module,
  )
import System.FilePath

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
        yolo = True,
        Asterius.Main.Task.hasMain = False,
        Asterius.Main.Task.verboseErr = True,
        extraRootSymbols = extra_syms
      }
