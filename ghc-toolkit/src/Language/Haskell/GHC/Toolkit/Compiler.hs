{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Compiler
  ( HaskellIR(..)
  , CmmIR(..)
  , Compiler
  , patch
  , withHaskellIR
  , withCmmIR
  , finalize
  ) where

import Cmm
import GHC
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
  , finalize :: Ghc ()
  }

instance Semigroup Compiler where
  c0 <> c1 =
    Compiler
      { patch =
          \mod_summary parsed_mod -> do
            parsed_mod' <- patch c0 mod_summary parsed_mod
            patch c1 mod_summary parsed_mod'
      , withHaskellIR = \mod_summary hs_ir -> do
          withHaskellIR c0 mod_summary hs_ir
          withHaskellIR c1 mod_summary hs_ir
      , withCmmIR = \cmm_ir -> do
          withCmmIR c0 cmm_ir
          withCmmIR c1 cmm_ir
      , finalize = do
          finalize c0
          finalize c1
      }

instance Monoid Compiler where
  mempty = Compiler
    { patch = const pure
    , withHaskellIR = \_ _ -> pure ()
    , withCmmIR = \_ -> pure ()
    , finalize = pure ()
    }
