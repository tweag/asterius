{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Toolkit.BootLibs
  ( bootLibsPath
  ) where

import Language.Haskell.TH.Syntax
import Paths_ghc_toolkit
import System.FilePath

bootLibsPath :: FilePath
bootLibsPath =
  $(do datadir <- runIO getDataDir
       liftString $ datadir </> "boot-libs")
