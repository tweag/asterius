{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Infer the type of an Asterius expression.
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
  Unary {unaryOp = op} -> [snd $ inferUnaryOp op]
  Binary {binaryOp = AddInt64} -> [I64]
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

-- | Infer the type of a unary operator as @(input_type, output_type)@.
inferUnaryOp :: UnaryOp -> (ValueType, ValueType)
inferUnaryOp = \case
  -- unop Int32
  ClzInt32 -> (I32, I32)
  CtzInt32 -> (I32, I32)
  PopcntInt32 -> (I32, I32)
  -- unop Int64
  ClzInt64 -> (I64, I64)
  CtzInt64 -> (I64, I64)
  PopcntInt64 -> (I64, I64)
  -- unop Float32
  NegFloat32 -> (F32, F32)
  AbsFloat32 -> (F32, F32)
  CeilFloat32 -> (F32, F32)
  FloorFloat32 -> (F32, F32)
  TruncFloat32 -> (F32, F32)
  NearestFloat32 -> (F32, F32)
  SqrtFloat32 -> (F32, F32)
  -- unop Float64
  NegFloat64 -> (F32, F32)
  AbsFloat64 -> (F64, F64)
  CeilFloat64 -> (F64, F64)
  FloorFloat64 -> (F64, F64)
  TruncFloat64 -> (F64, F64)
  NearestFloat64 -> (F64, F64)
  SqrtFloat64 -> (F64, F64)
  -- testop Int32
  EqZInt32 -> (I32, I32)
  -- testop Int64
  EqZInt64 -> (I64, I32)
  -- cvtop Int32
  WrapInt64 -> (I64, I32)
  TruncSFloat32ToInt32 -> (F32, I32)
  TruncUFloat32ToInt32 -> (F32, I32)
  ReinterpretFloat32 -> (F32, I32)
  TruncSFloat64ToInt32 -> (F64, I32)
  TruncUFloat64ToInt32 -> (F64, I32)
  -- cvtop Int64
  ExtendSInt32 -> (I32, I64)
  ExtendUInt32 -> (I32, I64)
  TruncSFloat32ToInt64 -> (F32, I64)
  TruncUFloat32ToInt64 -> (F32, I64)
  TruncSFloat64ToInt64 -> (F64, I64)
  TruncUFloat64ToInt64 -> (F64, I64)
  ReinterpretFloat64 -> (F64, I64)
  -- cvtop Float32
  ConvertSInt32ToFloat32 -> (I32, F32)
  ConvertUInt32ToFloat32 -> (I32, F32)
  ReinterpretInt32 -> (I32, F32)
  ConvertSInt64ToFloat32 -> (I64, F32)
  ConvertUInt64ToFloat32 -> (I64, F32)
  DemoteFloat64 -> (F64, F32)
  -- cvtop Float64
  ConvertSInt32ToFloat64 -> (I32, F64)
  ConvertUInt32ToFloat64 -> (I32, F64)
  ConvertSInt64ToFloat64 -> (I64, F64)
  ConvertUInt64ToFloat64 -> (I64, F64)
  ReinterpretInt64 -> (I64, F64)
  PromoteFloat32 -> (F32, F64)
