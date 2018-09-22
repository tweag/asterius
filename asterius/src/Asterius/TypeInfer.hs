{-# LANGUAGE RecordWildCards #-}

module Asterius.TypeInfer
  ( infer
  ) where

import Asterius.Types

infer :: Expression -> ValueType
infer expr =
  case expr of
    Block {..} -> valueType
    If {..} -> infer ifTrue
    Loop {} -> None
    Break {} -> None
    Switch {} -> None
    Call {..} -> valueType
    CallImport {..} -> valueType
    CallIndirect {} -> I64
    GetLocal {..} -> valueType
    SetLocal {..} -> None
    Load {..} -> valueType
    Store {} -> None
    ConstI32 {} -> I32
    ConstI64 {} -> I64
    ConstF32 {} -> F32
    ConstF64 {} -> F64
    Unary {unaryOp = WrapInt64} -> I32
    Unary {unaryOp = ExtendUInt32} -> I64
    Host {..} -> I32
    Nop -> None
    Unreachable -> None
    CFG {} -> None
    Unresolved {} -> I64
    UnresolvedOff {} -> I64
    UnresolvedGetLocal {..} ->
      case unresolvedLocalReg of
        UniqueLocalReg _ vt -> vt
        QuotRemI32X -> I32
        QuotRemI32Y -> I32
        QuotRemI64X -> I64
        QuotRemI64Y -> I64
    UnresolvedSetLocal {} -> None
    UnresolvedGetGlobal {..} ->
      case unresolvedGlobalReg of
        FloatReg {} -> F32
        DoubleReg {} -> F64
        _ -> I64
    UnresolvedSetGlobal {..} -> None
    EmitErrorMessage {..} -> valueType
    Null -> None
    _ -> error $ "Asterius.TypeInfer.infer: " <> show expr
