{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Compiler
  ( IR(..)
  , Compiler
  , patch
  , withIR
  , defaultCompiler
  ) where

import Cmm
import HscTypes
import PipelineMonad
import StgSyn
import TcRnTypes

data IR = IR
  { parsed :: HsParsedModule
  , typeChecked :: TcGblEnv
  , core :: CgGuts
  , stg :: [StgTopBinding]
  , cmm :: [CmmDecl]
  , cmmRaw :: [RawCmmDecl]
  }

data Compiler = Compiler
  { patch :: ModSummary -> HsParsedModule -> Hsc HsParsedModule
  , withIR :: ModSummary -> IR -> CompPipeline ()
  }

defaultCompiler :: Compiler
defaultCompiler = Compiler {patch = const pure, withIR = \_ _ -> pure ()}
