{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.WebAssembly.NIR
  ( ValueType(..)
  , FunctionType(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , Expression(..)
  , Function(..)
  , marshalFunction
  ) where

import Bindings.Binaryen.Raw
import qualified Data.ByteString.Short as SBS
import Data.Data
import Data.Serialize
import qualified Data.Vector as V
import GHC.Generics
import Language.WebAssembly.Internals
import UnliftIO
import UnliftIO.Foreign

data ValueType
  = None
  | I32
  | I64
  | F32
  | F64
  | Auto
  deriving (Show, Generic, Data)

instance Serialize ValueType

data FunctionType = FunctionType
  { name :: SBS.ShortByteString
  , returnType :: ValueType
  , paramTypes :: V.Vector ValueType
  } deriving (Show, Generic, Data)

instance Serialize FunctionType

data UnaryOp
  = ClzInt32
  | CtzInt32
  | PopcntInt32
  | NegFloat32
  | AbsFloat32
  | CeilFloat32
  | FloorFloat32
  | TruncFloat32
  | NearestFloat32
  | SqrtFloat32
  | EqZInt32
  | ClzInt64
  | CtzInt64
  | PopcntInt64
  | NegFloat64
  | AbsFloat64
  | CeilFloat64
  | FloorFloat64
  | TruncFloat64
  | NearestFloat64
  | SqrtFloat64
  | EqZInt64
  | ExtendSInt32
  | ExtendUInt32
  | WrapInt64
  | TruncSFloat32ToInt32
  | TruncSFloat32ToInt64
  | TruncUFloat32ToInt32
  | TruncUFloat32ToInt64
  | TruncSFloat64ToInt32
  | TruncSFloat64ToInt64
  | TruncUFloat64ToInt32
  | TruncUFloat64ToInt64
  | ReinterpretFloat32
  | ReinterpretFloat64
  | ConvertSInt32ToFloat32
  | ConvertSInt32ToFloat64
  | ConvertUInt32ToFloat32
  | ConvertUInt32ToFloat64
  | ConvertSInt64ToFloat32
  | ConvertSInt64ToFloat64
  | ConvertUInt64ToFloat32
  | ConvertUInt64ToFloat64
  | PromoteFloat32
  | DemoteFloat64
  | ReinterpretInt32
  | ReinterpretInt64
  deriving (Show, Generic, Data)

instance Serialize UnaryOp

data BinaryOp
  = AddInt32
  | SubInt32
  | MulInt32
  | DivSInt32
  | DivUInt32
  | RemSInt32
  | RemUInt32
  | AndInt32
  | OrInt32
  | XorInt32
  | ShlInt32
  | ShrUInt32
  | ShrSInt32
  | RotLInt32
  | RotRInt32
  | EqInt32
  | NeInt32
  | LtSInt32
  | LtUInt32
  | LeSInt32
  | LeUInt32
  | GtSInt32
  | GtUInt32
  | GeSInt32
  | GeUInt32
  | AddInt64
  | SubInt64
  | MulInt64
  | DivSInt64
  | DivUInt64
  | RemSInt64
  | RemUInt64
  | AndInt64
  | OrInt64
  | XorInt64
  | ShlInt64
  | ShrUInt64
  | ShrSInt64
  | RotLInt64
  | RotRInt64
  | EqInt64
  | NeInt64
  | LtSInt64
  | LtUInt64
  | LeSInt64
  | LeUInt64
  | GtSInt64
  | GtUInt64
  | GeSInt64
  | GeUInt64
  | AddFloat32
  | SubFloat32
  | MulFloat32
  | DivFloat32
  | CopySignFloat32
  | MinFloat32
  | MaxFloat32
  | EqFloat32
  | NeFloat32
  | LtFloat32
  | LeFloat32
  | GtFloat32
  | GeFloat32
  | AddFloat64
  | SubFloat64
  | MulFloat64
  | DivFloat64
  | CopySignFloat64
  | MinFloat64
  | MaxFloat64
  | EqFloat64
  | NeFloat64
  | LtFloat64
  | LeFloat64
  | GtFloat64
  | GeFloat64
  deriving (Show, Generic, Data)

instance Serialize BinaryOp

data Expression
  = ConstI32 Int32
  | ConstI64 Int64
  | ConstF32 Float
  | ConstF64 Double
  | ConstF32Bits Int32
  | ConstF64Bits Int64
  | Block { name :: SBS.ShortByteString
          , bodys :: V.Vector Expression
          , blockType :: ValueType }
  | If { condition, ifTrue, ifFalse :: Expression }
  | Loop { name :: SBS.ShortByteString
         , body :: Expression }
  | Break { name :: SBS.ShortByteString
          , condition, value :: Expression }
  | Switch { names :: V.Vector SBS.ShortByteString
           , defaultName :: SBS.ShortByteString
           , condition, value :: Expression }
  | GetLocal { index :: BinaryenIndex
             , localType :: ValueType }
  | SetLocal { index :: BinaryenIndex
             , value :: Expression }
  | TeeLocal { index :: BinaryenIndex
             , value :: Expression }
  | Unary { unaryOp :: UnaryOp
          , operand0 :: Expression }
  | Binary { binaryOp :: BinaryOp
           , operand0, operand1 :: Expression }
  | Return { value :: Expression }
  | Null
  deriving (Show, Generic, Data)

instance Serialize Expression

data Function = Function
  { name :: SBS.ShortByteString
  , functionType :: FunctionType
  , varTypes :: V.Vector ValueType
  , body :: Expression
  } deriving (Show, Generic, Data)

instance Serialize Function

marshalValueType :: ValueType -> BinaryenType
marshalValueType t =
  case t of
    None -> c_BinaryenTypeNone
    I32 -> c_BinaryenTypeInt32
    I64 -> c_BinaryenTypeInt64
    F32 -> c_BinaryenTypeFloat32
    F64 -> c_BinaryenTypeFloat64
    Auto -> c_BinaryenTypeAuto

marshalUnaryOp :: UnaryOp -> BinaryenOp
marshalUnaryOp op =
  case op of
    ClzInt32 -> c_BinaryenClzInt32
    CtzInt32 -> c_BinaryenCtzInt32
    PopcntInt32 -> c_BinaryenPopcntInt32
    NegFloat32 -> c_BinaryenNegFloat32
    AbsFloat32 -> c_BinaryenAbsFloat32
    CeilFloat32 -> c_BinaryenCeilFloat32
    FloorFloat32 -> c_BinaryenFloorFloat32
    TruncFloat32 -> c_BinaryenTruncFloat32
    NearestFloat32 -> c_BinaryenNearestFloat32
    SqrtFloat32 -> c_BinaryenSqrtFloat32
    EqZInt32 -> c_BinaryenEqZInt32
    ClzInt64 -> c_BinaryenClzInt64
    CtzInt64 -> c_BinaryenCtzInt64
    PopcntInt64 -> c_BinaryenPopcntInt64
    NegFloat64 -> c_BinaryenNegFloat64
    AbsFloat64 -> c_BinaryenAbsFloat64
    CeilFloat64 -> c_BinaryenCeilFloat64
    FloorFloat64 -> c_BinaryenFloorFloat64
    TruncFloat64 -> c_BinaryenTruncFloat64
    NearestFloat64 -> c_BinaryenNearestFloat64
    SqrtFloat64 -> c_BinaryenSqrtFloat64
    EqZInt64 -> c_BinaryenEqZInt64
    ExtendSInt32 -> c_BinaryenExtendSInt32
    ExtendUInt32 -> c_BinaryenExtendUInt32
    WrapInt64 -> c_BinaryenWrapInt64
    TruncSFloat32ToInt32 -> c_BinaryenTruncSFloat32ToInt32
    TruncSFloat32ToInt64 -> c_BinaryenTruncSFloat32ToInt64
    TruncUFloat32ToInt32 -> c_BinaryenTruncUFloat32ToInt32
    TruncUFloat32ToInt64 -> c_BinaryenTruncUFloat32ToInt64
    TruncSFloat64ToInt32 -> c_BinaryenTruncSFloat64ToInt32
    TruncSFloat64ToInt64 -> c_BinaryenTruncSFloat64ToInt64
    TruncUFloat64ToInt32 -> c_BinaryenTruncUFloat64ToInt32
    TruncUFloat64ToInt64 -> c_BinaryenTruncUFloat64ToInt64
    ReinterpretFloat32 -> c_BinaryenReinterpretFloat32
    ReinterpretFloat64 -> c_BinaryenReinterpretFloat64
    ConvertSInt32ToFloat32 -> c_BinaryenConvertSInt32ToFloat32
    ConvertSInt32ToFloat64 -> c_BinaryenConvertSInt32ToFloat64
    ConvertUInt32ToFloat32 -> c_BinaryenConvertUInt32ToFloat32
    ConvertUInt32ToFloat64 -> c_BinaryenConvertUInt32ToFloat64
    ConvertSInt64ToFloat32 -> c_BinaryenConvertSInt64ToFloat32
    ConvertSInt64ToFloat64 -> c_BinaryenConvertSInt64ToFloat64
    ConvertUInt64ToFloat32 -> c_BinaryenConvertUInt64ToFloat32
    ConvertUInt64ToFloat64 -> c_BinaryenConvertUInt64ToFloat64
    PromoteFloat32 -> c_BinaryenPromoteFloat32
    DemoteFloat64 -> c_BinaryenDemoteFloat64
    ReinterpretInt32 -> c_BinaryenReinterpretInt32
    ReinterpretInt64 -> c_BinaryenReinterpretInt64

marshalBinaryOp :: BinaryOp -> BinaryenOp
marshalBinaryOp op =
  case op of
    AddInt32 -> c_BinaryenAddInt32
    SubInt32 -> c_BinaryenSubInt32
    MulInt32 -> c_BinaryenMulInt32
    DivSInt32 -> c_BinaryenDivSInt32
    DivUInt32 -> c_BinaryenDivUInt32
    RemSInt32 -> c_BinaryenRemSInt32
    RemUInt32 -> c_BinaryenRemUInt32
    AndInt32 -> c_BinaryenAndInt32
    OrInt32 -> c_BinaryenOrInt32
    XorInt32 -> c_BinaryenXorInt32
    ShlInt32 -> c_BinaryenShlInt32
    ShrUInt32 -> c_BinaryenShrUInt32
    ShrSInt32 -> c_BinaryenShrSInt32
    RotLInt32 -> c_BinaryenRotLInt32
    RotRInt32 -> c_BinaryenRotRInt32
    EqInt32 -> c_BinaryenEqInt32
    NeInt32 -> c_BinaryenNeInt32
    LtSInt32 -> c_BinaryenLtSInt32
    LtUInt32 -> c_BinaryenLtUInt32
    LeSInt32 -> c_BinaryenLeSInt32
    LeUInt32 -> c_BinaryenLeUInt32
    GtSInt32 -> c_BinaryenGtSInt32
    GtUInt32 -> c_BinaryenGtUInt32
    GeSInt32 -> c_BinaryenGeSInt32
    GeUInt32 -> c_BinaryenGeUInt32
    AddInt64 -> c_BinaryenAddInt64
    SubInt64 -> c_BinaryenSubInt64
    MulInt64 -> c_BinaryenMulInt64
    DivSInt64 -> c_BinaryenDivSInt64
    DivUInt64 -> c_BinaryenDivUInt64
    RemSInt64 -> c_BinaryenRemSInt64
    RemUInt64 -> c_BinaryenRemUInt64
    AndInt64 -> c_BinaryenAndInt64
    OrInt64 -> c_BinaryenOrInt64
    XorInt64 -> c_BinaryenXorInt64
    ShlInt64 -> c_BinaryenShlInt64
    ShrUInt64 -> c_BinaryenShrUInt64
    ShrSInt64 -> c_BinaryenShrSInt64
    RotLInt64 -> c_BinaryenRotLInt64
    RotRInt64 -> c_BinaryenRotRInt64
    EqInt64 -> c_BinaryenEqInt64
    NeInt64 -> c_BinaryenNeInt64
    LtSInt64 -> c_BinaryenLtSInt64
    LtUInt64 -> c_BinaryenLtUInt64
    LeSInt64 -> c_BinaryenLeSInt64
    LeUInt64 -> c_BinaryenLeUInt64
    GtSInt64 -> c_BinaryenGtSInt64
    GtUInt64 -> c_BinaryenGtUInt64
    GeSInt64 -> c_BinaryenGeSInt64
    GeUInt64 -> c_BinaryenGeUInt64
    AddFloat32 -> c_BinaryenAddFloat32
    SubFloat32 -> c_BinaryenSubFloat32
    MulFloat32 -> c_BinaryenMulFloat32
    DivFloat32 -> c_BinaryenDivFloat32
    CopySignFloat32 -> c_BinaryenCopySignFloat32
    MinFloat32 -> c_BinaryenMinFloat32
    MaxFloat32 -> c_BinaryenMaxFloat32
    EqFloat32 -> c_BinaryenEqFloat32
    NeFloat32 -> c_BinaryenNeFloat32
    LtFloat32 -> c_BinaryenLtFloat32
    LeFloat32 -> c_BinaryenLeFloat32
    GtFloat32 -> c_BinaryenGtFloat32
    GeFloat32 -> c_BinaryenGeFloat32
    AddFloat64 -> c_BinaryenAddFloat64
    SubFloat64 -> c_BinaryenSubFloat64
    MulFloat64 -> c_BinaryenMulFloat64
    DivFloat64 -> c_BinaryenDivFloat64
    CopySignFloat64 -> c_BinaryenCopySignFloat64
    MinFloat64 -> c_BinaryenMinFloat64
    MaxFloat64 -> c_BinaryenMaxFloat64
    EqFloat64 -> c_BinaryenEqFloat64
    NeFloat64 -> c_BinaryenNeFloat64
    LtFloat64 -> c_BinaryenLtFloat64
    LeFloat64 -> c_BinaryenLeFloat64
    GtFloat64 -> c_BinaryenGtFloat64
    GeFloat64 -> c_BinaryenGeFloat64

marshalFunctionType ::
     MonadIO m => BinaryenModuleRef -> FunctionType -> m BinaryenFunctionTypeRef
marshalFunctionType m FunctionType {..} =
  liftIO $
  withSV (V.convert $ V.map marshalValueType paramTypes) $ \pts ptl ->
    withSBS name $ \np ->
      c_BinaryenAddFunctionType m np (marshalValueType returnType) pts ptl

marshalExpression ::
     MonadIO m => BinaryenModuleRef -> Expression -> m BinaryenExpressionRef
marshalExpression m e =
  liftIO $
  case e of
    ConstI32 x -> c_BinaryenConstInt32 m x
    ConstI64 x -> c_BinaryenConstInt64 m x
    ConstF32 x -> c_BinaryenConstFloat32 m x
    ConstF64 x -> c_BinaryenConstFloat64 m x
    ConstF32Bits x -> c_BinaryenConstFloat32Bits m x
    ConstF64Bits x -> c_BinaryenConstFloat64Bits m x
    Block {..} -> do
      bs <- fmap V.convert $ V.forM bodys $ marshalExpression m
      withSV bs $ \bsp bl ->
        withSBS name $ \np ->
          c_BinaryenBlock m np bsp bl (marshalValueType blockType)
    If {..} -> do
      c <- marshalExpression m condition
      t <- marshalExpression m ifTrue
      f <- marshalExpression m ifFalse
      c_BinaryenIf m c t f
    Loop {..} -> do
      b <- marshalExpression m body
      withSBS name $ \np -> c_BinaryenLoop m np b
    Break {..} -> do
      c <- marshalExpression m condition
      v <- marshalExpression m value
      withSBS name $ \np -> c_BinaryenBreak m np c v
    Switch {..} -> do
      c <- marshalExpression m condition
      v <- marshalExpression m value
      ns <- fmap V.convert $ V.forM names $ flip withSBS pure
      withSV ns $ \nsp nl ->
        withSBS defaultName $ \dn -> c_BinaryenSwitch m nsp nl dn c v
    GetLocal {..} -> c_BinaryenGetLocal m index $ marshalValueType localType
    SetLocal {..} -> do
      v <- marshalExpression m value
      c_BinaryenSetLocal m index v
    TeeLocal {..} -> do
      v <- marshalExpression m value
      c_BinaryenTeeLocal m index v
    Unary {..} -> do
      x <- marshalExpression m operand0
      c_BinaryenUnary m (marshalUnaryOp unaryOp) x
    Binary {..} -> do
      x <- marshalExpression m operand0
      y <- marshalExpression m operand1
      c_BinaryenBinary m (marshalBinaryOp binaryOp) x y
    Return {..} -> do
      v <- marshalExpression m value
      c_BinaryenReturn m v
    Null -> pure nullPtr

marshalFunction ::
     MonadIO m => BinaryenModuleRef -> Function -> m BinaryenFunctionRef
marshalFunction m Function {..} =
  liftIO $ do
    ft <- marshalFunctionType m functionType
    b <- marshalExpression m body
    withSV (V.convert $ V.map marshalValueType varTypes) $ \vtp vtl ->
      withSBS name $ \np -> c_BinaryenAddFunction m np ft vtp vtl b

instance Serialize SBS.ShortByteString where
  {-# INLINE put #-}
  put sbs = put (SBS.length sbs) *> putShortByteString sbs
  {-# INLINE get #-}
  get = get >>= getShortByteString

instance Serialize a => Serialize (V.Vector a) where
  {-# INLINE put #-}
  put v = put (V.length v) *> V.mapM_ put v
  {-# INLINE get #-}
  get = do
    len <- get
    V.replicateM len get
