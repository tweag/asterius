{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Toolkit.IROrphans
  ( setDynFlagsRef
  ) where

import BasicTypes
import CLabel
import Cmm
import CoAxiom
import CoreSyn
import CostCentre
import CostCentreState
import Data.IORef
import DataCon
import DynFlags
import ForeignCall
import Hoopl.Block
import Hoopl.Graph
import Literal
import Module
import Name
import Outputable
import PrimOp
import StgSyn
import System.IO.Unsafe
import Text.Show.Functions ()
import TyCoRep
import TyCon
import Var

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

deriving instance Show FunctionOrData

deriving instance
         (Show tyvar, Show argf) => Show (TyVarBndr tyvar argf)

instance Show Var where
  show = fakeShow "Var"

instance Show TyCon where
  show = fakeShow "TyCon"

deriving instance Show ArgFlag

deriving instance Show TyLit

deriving instance Show Role

deriving instance Show CoAxBranch

instance Show (Branches br) where
  show = show . fromBranches

deriving instance Show (CoAxiom br)

deriving instance Show UnivCoProvenance

deriving instance Show CoAxiomRule

deriving instance Show LeftOrRight

instance Show a => Show (IORef a) where
  show = show . unsafePerformIO . readIORef

deriving instance Show CoercionHole

deriving instance Show Coercion

deriving instance Show Type

deriving instance Show Literal

deriving instance Show occ => Show (GenStgArg occ)

instance Show DataCon where
  show = fakeShow "DataCon"

deriving instance Show PrimOpVecCat

deriving instance Show PrimOp

deriving instance Show PrimCall

deriving instance Show CCallTarget

deriving instance Show CCallSpec

deriving instance Show ForeignCall

deriving instance Show StgOp

deriving instance Show AltType

deriving instance Show AltCon

deriving instance
         (Show bndr, Show occ) => Show (GenStgExpr bndr occ)

instance Show CostCentreStack where
  show = fakeShow "CostCentreStack"

instance Show StgBinderInfo where
  show sbi =
    if satCallsOnly sbi
      then "SatCallsOnly"
      else "NoStgBinderInfo"

deriving instance Show UpdateFlag

deriving instance
         (Show bndr, Show occ) => Show (GenStgRhs bndr occ)

deriving instance
         (Show bndr, Show occ) => Show (GenStgBinding bndr occ)

deriving instance
         (Show bndr, Show occ) => Show (GenStgTopBinding bndr occ)

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
