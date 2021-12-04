{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Passes.GlobalRegs
  ( unresolvedGetGlobal,
    unresolvedSetGlobal,
  )
where

import Asterius.Types
import Language.Haskell.GHC.Toolkit.Constants

globalRegInfo :: UnresolvedGlobalReg -> (BinaryenIndex, Int, ValueType)
globalRegInfo gr = case gr of
  VanillaReg 1 -> (4, offset_StgRegTable_rR1, I32)
  VanillaReg 2 -> (4, offset_StgRegTable_rR2, I32)
  VanillaReg 3 -> (4, offset_StgRegTable_rR3, I32)
  VanillaReg 4 -> (4, offset_StgRegTable_rR4, I32)
  VanillaReg 5 -> (4, offset_StgRegTable_rR5, I32)
  VanillaReg 6 -> (4, offset_StgRegTable_rR6, I32)
  VanillaReg 7 -> (4, offset_StgRegTable_rR7, I32)
  VanillaReg 8 -> (4, offset_StgRegTable_rR8, I32)
  VanillaReg 9 -> (4, offset_StgRegTable_rR9, I32)
  VanillaReg 10 -> (4, offset_StgRegTable_rR10, I32)
  FloatReg 1 -> (4, offset_StgRegTable_rF1, F32)
  FloatReg 2 -> (4, offset_StgRegTable_rF2, F32)
  FloatReg 3 -> (4, offset_StgRegTable_rF3, F32)
  FloatReg 4 -> (4, offset_StgRegTable_rF4, F32)
  FloatReg 5 -> (4, offset_StgRegTable_rF5, F32)
  FloatReg 6 -> (4, offset_StgRegTable_rF6, F32)
  DoubleReg 1 -> (8, offset_StgRegTable_rD1, F64)
  DoubleReg 2 -> (8, offset_StgRegTable_rD2, F64)
  DoubleReg 3 -> (8, offset_StgRegTable_rD3, F64)
  DoubleReg 4 -> (8, offset_StgRegTable_rD4, F64)
  DoubleReg 5 -> (8, offset_StgRegTable_rD5, F64)
  DoubleReg 6 -> (8, offset_StgRegTable_rD6, F64)
  LongReg 1 -> (8, offset_StgRegTable_rL1, I64)
  Sp -> (4, offset_StgRegTable_rSp, I32)
  SpLim -> (4, offset_StgRegTable_rSpLim, I32)
  Hp -> (4, offset_StgRegTable_rHp, I32)
  HpLim -> (4, offset_StgRegTable_rHpLim, I32)
  CCCS -> (4, offset_StgRegTable_rCCCS, I32)
  CurrentTSO -> (4, offset_StgRegTable_rCurrentTSO, I32)
  CurrentNursery -> (4, offset_StgRegTable_rCurrentNursery, I32)
  HpAlloc -> (4, offset_StgRegTable_rHpAlloc, I32)
  EagerBlackholeInfo -> (4, rf + offset_StgFunTable_stgEagerBlackholeInfo, I32)
  GCEnter1 -> (4, rf + offset_StgFunTable_stgGCEnter1, I32)
  GCFun -> (4, rf + offset_StgFunTable_stgGCFun, I32)
  _ ->
    error $
      "Asterius.Passes.GlobalRegs.resolveGlobalRegs: Unsupported global reg "
        <> show gr
  where
    rf = offset_Capability_f - offset_Capability_r

mainCap :: Expression
mainCap = Symbol {unresolvedSymbol = "MainCapability", symbolOffset = 0}

baseReg :: Expression
baseReg = mainCap {symbolOffset = offset_Capability_r}

unresolvedGetGlobal :: UnresolvedGlobalReg -> Expression
unresolvedGetGlobal gr = case gr of
  BaseReg -> baseReg
  _ -> Load
    { signed = False,
      bytes = b,
      offset = fromIntegral $ offset_Capability_r + o,
      valueType = vt,
      ptr = mainCap
    }
    where
      (b, o, vt) = globalRegInfo gr

unresolvedSetGlobal :: UnresolvedGlobalReg -> Expression -> Expression
unresolvedSetGlobal gr v = case gr of
  BaseReg -> Nop
  _ -> Store
    { bytes = b,
      offset = fromIntegral $ offset_Capability_r + o,
      ptr = mainCap,
      value = v,
      valueType = vt
    }
    where
      (b, o, vt) = globalRegInfo gr
