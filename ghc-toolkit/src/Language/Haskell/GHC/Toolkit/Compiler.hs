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
import HscTypes
import PipelineMonad
import Stream (Stream)

data HaskellIR = HaskellIR
  { sptEntries :: [SptEntry],
    cmmRaw :: Stream IO Cmm.RawCmmGroup ()
  }

newtype CmmIR = CmmIR
  { cmmRaw :: Stream IO Cmm.RawCmmGroup ()
  }

data Compiler = Compiler
  { withHaskellIR :: ModSummary -> HaskellIR -> FilePath -> CompPipeline (),
    withCmmIR :: DynFlags -> Module -> CmmIR -> FilePath -> IO ()
  }
