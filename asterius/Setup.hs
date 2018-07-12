{-# OPTIONS_GHC -Wall #-}

import Distribution.Simple
import Language.Haskell.GHC.Toolkit.GenPaths

main :: IO ()
main =
  defaultMainWithHooks $
  genPaths
    GenPathsOptions {targetModuleName = "BuildInfo_asterius"}
    simpleUserHooks
