{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Toolkit.Orphans.Data
  (
  ) where

import CLabel
import Cmm
import CostCentre
import Data.Data
import ForeignCall
import PrimOp
import StgSyn
import TyCon
import Unique

deriving instance Data occ => Data (GenStgArg occ)

deriving instance Data Width

deriving instance Data PrimOpVecCat

deriving instance Data PrimOp

deriving instance Data PrimCall

uniqueConstr :: Constr
uniqueConstr = mkConstr uniqueDataType "Unique" [] Prefix

uniqueDataType :: DataType
uniqueDataType = mkDataType "Unique" [uniqueConstr]

instance Data Unique where
  gunfold _ _ _ = error "gunfold unsupported for Unique"
  toConstr _ = uniqueConstr
  dataTypeOf _ = uniqueDataType

deriving instance Data CCallSpec

deriving instance Data ForeignCall

deriving instance Data StgOp

deriving instance Data PrimElemRep

deriving instance Data PrimRep

deriving instance Data AltType

deriving instance
         (Data bndr, Data occ) => Data (GenStgExpr bndr occ)

deriving instance Data UpdateFlag

costCentreStackConstr :: Constr
costCentreStackConstr =
  mkConstr costCentreStackDataType "CostCentreStack" [] Prefix

costCentreStackDataType :: DataType
costCentreStackDataType = mkDataType "CostCentreStack" [costCentreStackConstr]

instance Data CostCentreStack where
  gunfold _ _ _ = error "gunfold unsupported for CostCentreStack"
  toConstr _ = costCentreStackConstr
  dataTypeOf _ = costCentreStackDataType

deriving instance
         (Data bndr, Data occ) => Data (GenStgRhs bndr occ)

deriving instance
         (Data bndr, Data occ) => Data (GenStgBinding bndr occ)

deriving instance
         (Data bndr, Data occ) => Data (GenStgTopBinding bndr occ)

deriving instance Data VGcPtr

deriving instance Data GlobalReg

cLabelConstr :: Constr
cLabelConstr = mkConstr cLabelDataType "CLabel" [] Prefix

cLabelDataType :: DataType
cLabelDataType = mkDataType "CLabel" [cLabelConstr]

instance Data CLabel where
  gunfold _ _ _ = error "gunfold unsupported for CLabel"
  toConstr _ = cLabelConstr
  dataTypeOf _ = cLabelDataType

deriving instance Data SectionType

deriving instance Data Section

deriving instance
         (Data d, Data h, Data g) => Data (GenCmmDecl d h g)
