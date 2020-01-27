{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Toolkit.Orphans.Show
  ( setDynFlagsRef,
  )
where

import CLabel
import Cmm
import CoAxiom
import Control.Monad.IO.Class
import CostCentre
import CostCentreState
import Data.IORef
import ForeignCall
import GhcPlugins
import Hoopl.Block
import Hoopl.Graph
import PrimOp
import SMRep
import StgSyn
import System.IO.Unsafe
import Text.Show.Functions ()
import TyCoRep
import UniqDSet

{-# NOINLINE dynFlagsRef #-}
dynFlagsRef :: IORef DynFlags
dynFlagsRef = unsafePerformIO $ newIORef unsafeGlobalDynFlags

setDynFlagsRef :: MonadIO m => DynFlags -> m ()
setDynFlagsRef = liftIO . atomicWriteIORef dynFlagsRef

fakeShow :: Outputable a => String -> a -> String
fakeShow tag val = unsafePerformIO $ do
  dflags <- readIORef dynFlagsRef
  pure $
    "("
      ++ tag
      ++ " "
      ++ show (showSDoc dflags $ pprCode AsmStyle $ ppr val)
      ++ ")"

instance Outputable SDoc where
  ppr = id

instance Show SDoc where
  show = fakeShow "SDoc"

deriving instance Show ForeignStubs

deriving instance Show InstalledUnitId

deriving instance Show HpcInfo

instance Show ModBreaks where
  show = const "ModBreaks"

deriving instance Show SptEntry

deriving instance Show CgGuts

deriving instance Show b => Show (Expr b)

deriving instance Show b => Show (Bind b)

deriving instance Show FunctionOrData

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

deriving instance Show MCoercionN

deriving instance Show Coercion

deriving instance (Show var, Show argf) => Show (VarBndr var argf)

deriving instance Show Type

deriving instance Show LitNumType

deriving instance Show Literal

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

instance Show NoExtSilent where
  show _ = "NoExtSilent"

deriving instance Show StgArg

deriving instance
  ( Show (BinderP pass),
    Show (XLet pass),
    Show (XLetNoEscape pass),
    Show (XRhsClosure pass)
  ) =>
  Show (GenStgExpr pass)

instance Show CostCentreStack where
  show = fakeShow "CostCentreStack"

deriving instance Show UpdateFlag

deriving instance
  ( Show (BinderP pass),
    Show (XLet pass),
    Show (XLetNoEscape pass),
    Show (XRhsClosure pass)
  ) =>
  Show (GenStgRhs pass)

deriving instance
  ( Show (BinderP pass),
    Show (XLet pass),
    Show (XLetNoEscape pass),
    Show (XRhsClosure pass)
  ) =>
  Show (GenStgBinding pass)

deriving instance
  ( Show (BinderP pass),
    Show (XLet pass),
    Show (XLetNoEscape pass),
    Show (XRhsClosure pass)
  ) =>
  Show (GenStgTopBinding pass)

instance Show Name where
  show = fakeShow "Name"

instance Show CLabel where
  show = fakeShow "CLabel"

deriving instance Show Section

deriving instance Show CmmLit

deriving instance Show CmmStatic

deriving instance Show CmmStatics

deriving instance Show t => Show (MaybeO ex t)

instance Show (n O O) => Show (Block n O O) where
  show = show . blockToList

instance (Show (n C O), Show (n O O), Show (n O C)) => Show (Block n C C) where
  show (BlockCC h b t) =
    "BlockCC (" ++ show h ++ ") (" ++ show b ++ ") (" ++ show t ++ ")"

instance
  (Show (block n C C), Show (n C O), Show (n O O), Show (n O C)) =>
  Show (Graph' block n C C)
  where
  show (GMany NothingO lm NothingO) = show $ bodyList lm

deriving instance
  (Show (n C O), Show (n O O), Show (n O C)) => Show (GenCmmGraph n)

deriving instance Show CmmTickScope

instance Show ModuleName where
  show = show . moduleNameString

instance Show Module where
  show (Module u m) =
    "Module \"" ++ unitIdString u ++ "\" \"" ++ moduleNameString m ++ "\""

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

deriving instance Show ArgDescr

deriving instance Show ClosureTypeInfo

deriving instance Show SMRep

deriving instance Show ProfilingInfo

instance Show StgHalfWord where
  show = show . fromStgHalfWord

deriving instance Show CmmInfoTable

deriving instance Show CmmStackInfo

deriving instance Show CmmTopInfo

instance Show a => Show (UniqDSet a) where
  showsPrec p = showsPrec p . uniqDSetToList
