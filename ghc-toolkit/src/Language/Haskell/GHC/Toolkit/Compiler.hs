{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Compiler
  ( HaskellIR(..)
  , CmmIR(..)
  , Compiler(..)
  ) where

import Cmm
import GHC
import HscTypes
import PipelineMonad
import StgSyn
import TcRnTypes

data HaskellIR = HaskellIR
  { parsed :: HsParsedModule
  , typechecked :: TcGblEnv
  , core :: CgGuts
  , stg :: [CgStgTopBinding]
  , cmm :: [[CmmDecl]]
  , cmmRaw :: [[RawCmmDecl]]
  }

data CmmIR = CmmIR
  { cmm :: [[CmmDecl]]
  , cmmRaw :: [[RawCmmDecl]]
  }

data Compiler = Compiler
  { patchParsed :: ModSummary -> HsParsedModule -> Hsc HsParsedModule
  , patchTypechecked :: ModSummary -> TcGblEnv -> Hsc TcGblEnv
  , withHaskellIR :: ModSummary -> HaskellIR -> FilePath -> CompPipeline ()
  , withCmmIR :: CmmIR -> FilePath -> CompPipeline ()
  , finalize :: Ghc ()
  }

instance Semigroup Compiler where
  c0 <> c1 =
    Compiler
      { patchParsed =
          \mod_summary parsed_mod -> do
            parsed_mod' <- patchParsed c0 mod_summary parsed_mod
            patchParsed c1 mod_summary parsed_mod'
      , patchTypechecked =
          \mod_summary tc_mod -> do
            tc_mod' <- patchTypechecked c0 mod_summary tc_mod
            patchTypechecked c1 mod_summary tc_mod'
      , withHaskellIR =
          \mod_summary hs_ir obj_path -> do
            withHaskellIR c0 mod_summary hs_ir obj_path
            withHaskellIR c1 mod_summary hs_ir obj_path
      , withCmmIR =
          \cmm_ir obj_path -> do
            withCmmIR c0 cmm_ir obj_path
            withCmmIR c1 cmm_ir obj_path
      , finalize =
          do finalize c0
             finalize c1
      }

instance Monoid Compiler where
  mempty =
    Compiler
      { patchParsed = const pure
      , patchTypechecked = const pure
      , withHaskellIR = \_ _ _ -> pure ()
      , withCmmIR = \_ _ -> pure ()
      , finalize = pure ()
      }
