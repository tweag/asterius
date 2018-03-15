{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Compiler
  ( IR(..)
  , Compiler(..)
  , defaultCompiler
  ) where

import Cmm
import HscTypes
import PipelineMonad
import StgSyn

data IR = IR
  { core :: CgGuts
  , stg :: [StgTopBinding]
  , cmmRaw :: [RawCmmDecl]
  }

newtype Compiler = Compiler
  { withIR :: ModSummary -> IR -> CompPipeline ()
  }

defaultCompiler :: Compiler
defaultCompiler = Compiler {withIR = \_ _ -> pure ()}
