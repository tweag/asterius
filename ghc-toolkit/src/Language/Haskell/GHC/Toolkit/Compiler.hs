{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Compiler
  ( HaskellIR(..)
  , CmmIR(..)
  , Compiler
  , patch
  , withHaskellIR
  , withCmmIR
  , defaultCompiler
  ) where

import Cmm
import HscTypes
import PipelineMonad
import StgSyn
import TcRnTypes

data HaskellIR = HaskellIR
  { parsed :: HsParsedModule
  , typeChecked :: TcGblEnv
  , core :: CgGuts
  , stg :: [StgTopBinding]
  , cmm :: [CmmDecl]
  , cmmRaw :: [RawCmmDecl]
  }

data CmmIR = CmmIR
  { cmm :: [CmmDecl]
  , cmmRaw :: [RawCmmDecl]
  }

data Compiler = Compiler
  { patch :: ModSummary -> HsParsedModule -> Hsc HsParsedModule
  , withHaskellIR :: ModSummary -> HaskellIR -> CompPipeline ()
  , withCmmIR :: CmmIR -> CompPipeline ()
  }

defaultCompiler :: Compiler
defaultCompiler =
  Compiler
    { patch = const pure
    , withHaskellIR = \_ _ -> pure ()
    , withCmmIR = \_ -> pure ()
    }
