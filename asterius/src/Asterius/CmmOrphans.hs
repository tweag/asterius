{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Asterius.CmmOrphans
  ( setDynFlagsRef
  ) where

import CLabel
import Cmm
import CoreSyn
import CostCentre
import CostCentreState
import Data.IORef
import DynFlags
import ForeignCall
import Hoopl.Block
import Hoopl.Graph
import Module
import Name
import Outputable
import System.IO.Unsafe

{-# NOINLINE dynFlagsRef #-}
dynFlagsRef :: IORef DynFlags
dynFlagsRef = unsafePerformIO $ newIORef unsafeGlobalDynFlags

setDynFlagsRef :: DynFlags -> IO ()
setDynFlagsRef = writeIORef dynFlagsRef

fakeShow :: Outputable a => String -> a -> String
fakeShow tag val =
  unsafePerformIO $ do
    dflags <- readIORef dynFlagsRef
    pure $
      "(" ++
      tag ++ " " ++ show (showSDoc dflags $ pprCode AsmStyle $ ppr val) ++ ")"

instance Show Name where
  show = fakeShow "Name"

instance Show CLabel where
  show = fakeShow "CLabel"

deriving instance Show Section

deriving instance Show CmmLit

deriving instance Show CmmStatic

deriving instance Show CmmStatics

deriving instance Show t => Show (MaybeO ex t)

deriving instance
         (Show (block n C C), Show (block n C O), Show (block n O C),
          Show (block n O O)) =>
         Show (Graph' block n e x)

deriving instance
         (Show (n C O), Show (n O C), Show (n O O)) => Show (Block n e x)

deriving instance
         (Show (n C O), Show (n O C), Show (n O O)) => Show (GenCmmGraph n)

deriving instance Show CmmTickScope

instance Show ModuleName where
  show = moduleNameString

deriving instance Show Module

instance Show CostCentreIndex where
  show = show . unCostCentreIndex

deriving instance Show CCFlavour

deriving instance Show CostCentre

deriving instance Show id => Show (Tickish id)

instance Show CmmType where
  show = fakeShow "CmmType"

deriving instance Show LocalReg

deriving instance Show CmmReg

deriving instance Show Area

deriving instance Show CmmExpr

deriving instance Show CCallConv

deriving instance Show ForeignHint

deriving instance Show CmmReturnInfo

deriving instance Show ForeignConvention

deriving instance Show ForeignTarget

deriving instance Show (CmmNode e x)

deriving instance
         (Show d, Show h, Show g) => Show (GenCmmDecl d h g)
