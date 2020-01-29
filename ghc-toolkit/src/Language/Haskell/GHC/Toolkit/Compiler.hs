{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Compiler
  ( HaskellIR (..),
    CmmIR (..),
    Compiler (..),
  )
where

import Cmm
import GHC
import PipelineMonad
import StgSyn

data HaskellIR
  = HaskellIR
      { stg :: [CgStgTopBinding],
        cmmRaw :: [[RawCmmDecl]]
      }

newtype CmmIR
  = CmmIR
      { cmmRaw :: [[RawCmmDecl]]
      }

data Compiler
  = Compiler
      { withHaskellIR :: ModSummary -> HaskellIR -> FilePath -> CompPipeline (),
        withCmmIR :: CmmIR -> FilePath -> CompPipeline ()
      }

instance Semigroup Compiler where
  c0 <> c1 = Compiler
    { withHaskellIR = \mod_summary hs_ir obj_path -> do
        withHaskellIR c0 mod_summary hs_ir obj_path
        withHaskellIR c1 mod_summary hs_ir obj_path,
      withCmmIR = \cmm_ir obj_path -> do
        withCmmIR c0 cmm_ir obj_path
        withCmmIR c1 cmm_ir obj_path
    }

instance Monoid Compiler where
  mempty =
    Compiler {withHaskellIR = \_ _ _ -> pure (), withCmmIR = \_ _ -> pure ()}
