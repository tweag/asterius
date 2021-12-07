module Language.Haskell.GHC.Toolkit.Compiler
  ( CmmIR (..),
    Compiler (..),
  )
where

import Cmm
import GHC
import HscTypes
import Stream (Stream)

newtype CmmIR = CmmIR
  { cmmRaw :: Stream IO Cmm.RawCmmGroup ()
  }

newtype Compiler = Compiler
  { withCmmIR :: DynFlags -> Module -> CmmIR -> FilePath -> IO ()
  }
