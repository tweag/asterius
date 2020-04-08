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
  Binary {binaryOp = op} -> [snd $ inferBinaryOp op]
  Host {} -> [I32]
  Nop -> []
  Unreachable -> []
  CFG {} -> []
  Symbol {} -> [I64]
  UnresolvedGetLocal {..} -> [typeOfUnresolvedLocalReg unresolvedLocalReg]
  UnresolvedSetLocal {} -> []
  -- Unhandled cases
  TeeLocal {} -> error $ "Asterius.TypeInfer.infer: " <> show expr
  Drop {} -> error $ "Asterius.TypeInfer.infer: " <> show expr
  ReturnCall {} -> error $ "Asterius.TypeInfer.infer: " <> show expr
  ReturnCallIndirect {} -> error $ "Asterius.TypeInfer.infer: " <> show expr
  Barf {} -> error $ "Asterius.TypeInfer.infer: " <> show expr

-- | Compute/extract the type of an 'UnresolvedLocalReg'.
typeOfUnresolvedLocalReg :: UnresolvedLocalReg -> ValueType
typeOfUnresolvedLocalReg = \case
  UniqueLocalReg _ vt -> vt
  QuotRemI32X -> I32
  QuotRemI32Y -> I32
  QuotRemI64X -> I64
  QuotRemI64Y -> I64

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

-- | Infer the type of a binary operator as
-- @((input_type_1, input_type_2), output_type)@.
inferBinaryOp :: BinaryOp -> ((ValueType, ValueType), ValueType)
inferBinaryOp = \case
  -- binop Int32
  AddInt32 -> i32BinopType
  SubInt32 -> i32BinopType
  MulInt32 -> i32BinopType
  DivSInt32 -> i32BinopType
  DivUInt32 -> i32BinopType
  RemSInt32 -> i32BinopType
  RemUInt32 -> i32BinopType
  AndInt32 -> i32BinopType
  OrInt32 -> i32BinopType
  XorInt32 -> i32BinopType
  ShlInt32 -> i32BinopType
  ShrSInt32 -> i32BinopType
  ShrUInt32 -> i32BinopType
  RotLInt32 -> i32BinopType
  RotRInt32 -> i32BinopType
  -- binop Int64
  AddInt64 -> i64BinopType
  SubInt64 -> i64BinopType
  MulInt64 -> i64BinopType
  DivSInt64 -> i64BinopType
  DivUInt64 -> i64BinopType
  RemSInt64 -> i64BinopType
  RemUInt64 -> i64BinopType
  AndInt64 -> i64BinopType
  OrInt64 -> i64BinopType
  XorInt64 -> i64BinopType
  ShlInt64 -> i64BinopType
  ShrSInt64 -> i64BinopType
  ShrUInt64 -> i64BinopType
  RotLInt64 -> i64BinopType
  RotRInt64 -> i64BinopType
  -- relop Int32
  EqInt32 -> i32RelopType
  NeInt32 -> i32RelopType
  LtSInt32 -> i32RelopType
  LtUInt32 -> i32RelopType
  GtSInt32 -> i32RelopType
  GtUInt32 -> i32RelopType
  LeSInt32 -> i32RelopType
  LeUInt32 -> i32RelopType
  GeSInt32 -> i32RelopType
  GeUInt32 -> i32RelopType
  -- relop Int64
  EqInt64 -> i64RelopType
  NeInt64 -> i64RelopType
  LtSInt64 -> i64RelopType
  LtUInt64 -> i64RelopType
  GtSInt64 -> i64RelopType
  GtUInt64 -> i64RelopType
  LeSInt64 -> i64RelopType
  LeUInt64 -> i64RelopType
  GeSInt64 -> i64RelopType
  GeUInt64 -> i64RelopType
  -- binop Float32
  AddFloat32 -> f32BinopType
  SubFloat32 -> f32BinopType
  MulFloat32 -> f32BinopType
  DivFloat32 -> f32BinopType
  MinFloat32 -> f32BinopType
  MaxFloat32 -> f32BinopType
  CopySignFloat32 -> f32BinopType
  -- binop Float64
  AddFloat64 -> f64BinopType
  SubFloat64 -> f64BinopType
  MulFloat64 -> f64BinopType
  DivFloat64 -> f64BinopType
  MinFloat64 -> f64BinopType
  MaxFloat64 -> f64BinopType
  CopySignFloat64 -> f64BinopType
  -- relop Float32
  EqFloat32 -> f32RelopType
  NeFloat32 -> f32RelopType
  LtFloat32 -> f32RelopType
  GtFloat32 -> f32RelopType
  LeFloat32 -> f32RelopType
  GeFloat32 -> f32RelopType
  -- relop Float64
  EqFloat64 -> f64RelopType
  NeFloat64 -> f64RelopType
  LtFloat64 -> f64RelopType
  GtFloat64 -> f64RelopType
  LeFloat64 -> f64RelopType
  GeFloat64 -> f64RelopType
  where
    i32BinopType = ((I32, I32), I32)
    i64BinopType = ((I64, I64), I64)
    i32RelopType = ((I32, I32), I32)
    i64RelopType = ((I64, I64), I32) -- NOTE: also I32 result
    f32BinopType = ((F32, F32), F32)
    f64BinopType = ((F64, F64), F64)
    f32RelopType = ((F32, F32), I32) -- NOTE: also I32 result
    f64RelopType = ((F64, F64), I32) -- NOTE: also I32 result
