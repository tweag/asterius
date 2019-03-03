{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.GlobalRegs
  ( resolveGlobalRegs
  ) where

import Asterius.Internals.SYB
import Asterius.Types
import Language.Haskell.GHC.Toolkit.Constants
import Type.Reflection

{-# INLINABLE resolveGlobalRegs #-}
resolveGlobalRegs :: Monad m => GenericM m
resolveGlobalRegs t =
  pure $
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        UnresolvedGetGlobal {..} ->
          case unresolvedGlobalReg of
            BaseReg -> baseReg
            _ ->
              Load
                { signed = False
                , bytes = b
                , offset = fromIntegral $ offset_Capability_r + o
                , valueType = vt
                , ptr = mainCap32
                }
              where (b, o, vt) = globalRegInfo unresolvedGlobalReg
        UnresolvedSetGlobal {..} ->
          case unresolvedGlobalReg of
            BaseReg -> error "Asterius.Passes.GlobalRegs: Assignment to BaseReg"
            _ ->
              Store
                { bytes = b
                , offset = fromIntegral $ offset_Capability_r + o
                , ptr = mainCap32
                , value = value
                , valueType = vt
                }
              where (b, o, vt) = globalRegInfo unresolvedGlobalReg
        _ -> t
    _ -> t
  where
    mainCap =
      Symbol
        { unresolvedSymbol = "MainCapability"
        , symbolOffset = 0
        , resolvedSymbol = Nothing
        }
    mainCap32 = Unary {unaryOp = WrapInt64, operand0 = mainCap}
    baseReg = mainCap {symbolOffset = offset_Capability_r}
    globalRegInfo gr =
      case gr of
        VanillaReg 1 -> (8, offset_StgRegTable_rR1, I64)
        VanillaReg 2 -> (8, offset_StgRegTable_rR2, I64)
        VanillaReg 3 -> (8, offset_StgRegTable_rR3, I64)
        VanillaReg 4 -> (8, offset_StgRegTable_rR4, I64)
        VanillaReg 5 -> (8, offset_StgRegTable_rR5, I64)
        VanillaReg 6 -> (8, offset_StgRegTable_rR6, I64)
        VanillaReg 7 -> (8, offset_StgRegTable_rR7, I64)
        VanillaReg 8 -> (8, offset_StgRegTable_rR8, I64)
        VanillaReg 9 -> (8, offset_StgRegTable_rR9, I64)
        VanillaReg 10 -> (8, offset_StgRegTable_rR10, I64)
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
        Sp -> (8, offset_StgRegTable_rSp, I64)
        SpLim -> (8, offset_StgRegTable_rSpLim, I64)
        Hp -> (8, offset_StgRegTable_rHp, I64)
        HpLim -> (8, offset_StgRegTable_rHpLim, I64)
        CCCS -> (8, offset_StgRegTable_rCCCS, I64)
        CurrentTSO -> (8, offset_StgRegTable_rCurrentTSO, I64)
        CurrentNursery -> (8, offset_StgRegTable_rCurrentNursery, I64)
        HpAlloc -> (8, offset_StgRegTable_rHpAlloc, I64)
        EagerBlackholeInfo ->
          (8, rf + offset_StgFunTable_stgEagerBlackholeInfo, I64)
        GCEnter1 -> (8, rf + offset_StgFunTable_stgGCEnter1, I64)
        GCFun -> (8, rf + offset_StgFunTable_stgGCFun, I64)
        _ ->
          error $
          "Asterius.Passes.GlobalRegs.resolveGlobalRegs: Unsupported global reg " <>
          show gr
      where
        rf = offset_Capability_f - offset_Capability_r
