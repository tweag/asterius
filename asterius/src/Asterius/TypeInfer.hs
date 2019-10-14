{-# LANGUAGE RecordWildCards #-}

module Asterius.TypeInfer
  ( infer,
  )
where

import Asterius.Types

infer :: Expression -> [ValueType]
infer expr = case expr of
  Block {..} -> blockReturnTypes
  If {..} -> infer ifTrue
  Loop {} -> []
  Break {} -> []
  Switch {} -> []
  Call {..} -> callReturnTypes
  CallImport {..} -> callImportReturnTypes
  CallIndirect {} -> [I64]
  GetLocal {..} -> [valueType]
  SetLocal {} -> []
  Load {..} -> [valueType]
  Store {} -> []
  ConstI32 {} -> [I32]
  ConstI64 {} -> [I64]
  ConstF32 {} -> [F32]
  ConstF64 {} -> [F64]
  Unary {unaryOp = WrapInt64} -> [I32]
  Unary {unaryOp = ExtendUInt32} -> [I64]
  Unary {unaryOp = ConvertUInt64ToFloat64} -> [F64]
  Unary {unaryOp = ConvertSInt64ToFloat64} -> [F64]
  Unary {unaryOp = TruncUFloat64ToInt64} -> [I64]
  Unary {unaryOp = TruncSFloat64ToInt64} -> [I64]
  Host {} -> [I32]
  Nop -> []
  Unreachable -> []
  CFG {} -> []
  Symbol {} -> [I64]
  UnresolvedGetLocal {..} -> case unresolvedLocalReg of
    UniqueLocalReg _ vt -> [vt]
    QuotRemI32X -> [I32]
    QuotRemI32Y -> [I32]
    QuotRemI64X -> [I64]
    QuotRemI64Y -> [I64]
  UnresolvedSetLocal {} -> []
  _ -> error $ "Asterius.TypeInfer.infer: " <> show expr
