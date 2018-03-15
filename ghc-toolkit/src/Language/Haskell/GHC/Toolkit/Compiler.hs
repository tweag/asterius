{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Compiler
  ( IR(..)
  , Compiler(..)
  ) where

import Cmm
import HscTypes
import StgSyn

data IR = IR
  { core :: CgGuts
  , stg :: [StgTopBinding]
  , cmmRaw :: [RawCmmDecl]
  }

newtype Compiler ctx = Compiler
  { runCompiler :: ctx -> ModSummary -> IR -> IO ()
  }
