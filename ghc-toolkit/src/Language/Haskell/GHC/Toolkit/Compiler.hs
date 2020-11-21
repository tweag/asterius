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

newtype HaskellIR = HaskellIR
  { cgGuts :: CgGuts
  }

newtype CmmIR = CmmIR
  { cmmRaw :: Stream IO Cmm.RawCmmGroup ()
  }

data Compiler = Compiler
  { withHaskellIR :: DynFlags -> Module -> HaskellIR -> FilePath -> IO (),
    withCmmIR :: DynFlags -> Module -> CmmIR -> FilePath -> IO ()
  }
