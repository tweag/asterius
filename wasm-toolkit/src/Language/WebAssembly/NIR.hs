{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.WebAssembly.NIR
  ( BinaryenIndex
  , ValueType(..)
  , FunctionType(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , HostOp(..)
  , AtomicRMWOp(..)
  , Expression(..)
  , Function(..)
  , FunctionImport(..)
  , TableImport(..)
  , GlobalImport(..)
  , FunctionExport(..)
  , TableExport(..)
  , GlobalExport(..)
  , Global(..)
  , FunctionTable(..)
  , DataSegment(..)
  , Memory(..)
  , Module(..)
  , RelooperAddBlock(..)
  , RelooperAddBranch(..)
  , RelooperBlock(..)
  , RelooperRun(..)
  , MarshalError(..)
  , emptyModule
  , marshalModule
  ) where

import Bindings.Binaryen.Raw hiding (RelooperBlock)
import Control.DeepSeq
import qualified Data.ByteString.Short as SBS
import Data.Data
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Serialize
import Data.Traversable
import qualified Data.Vector as V
import GHC.Generics (Generic)
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

instance NFData ValueType

data FunctionType = FunctionType
  { returnType :: ValueType
  , paramTypes :: V.Vector ValueType
  } deriving (Show, Generic, Data)

instance Serialize FunctionType

instance NFData FunctionType

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

instance NFData UnaryOp

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

instance NFData BinaryOp

data HostOp
  = PageSize
  | CurrentMemory
  | GrowMemory
  | HasFeature
  deriving (Show, Generic, Data)

instance Serialize HostOp

instance NFData HostOp

data AtomicRMWOp
  = AtomicRMWAdd
  | AtomicRMWSub
  | AtomicRMWAnd
  | AtomicRMWOr
  | AtomicRMWXor
  | AtomicRMWXchg
  deriving (Show, Generic, Data)

instance Serialize AtomicRMWOp

instance NFData AtomicRMWOp

data Expression
  = Block { name :: SBS.ShortByteString
          , bodys :: V.Vector Expression
          , valueType :: ValueType }
  | If { condition, ifTrue, ifFalse :: Expression }
  | Loop { name :: SBS.ShortByteString
         , body :: Expression }
  | Break { name :: SBS.ShortByteString
          , condition, value :: Expression }
  | Switch { names :: V.Vector SBS.ShortByteString
           , defaultName :: SBS.ShortByteString
           , condition, value :: Expression }
  | Call { target :: SBS.ShortByteString
         , operands :: V.Vector Expression
         , valueType :: ValueType }
  | CallImport { target :: SBS.ShortByteString
               , operands :: V.Vector Expression
               , valueType :: ValueType }
  | CallIndirect { indirectTarget :: Expression
                 , operands :: V.Vector Expression
                 , typeName :: SBS.ShortByteString }
  | GetLocal { index :: BinaryenIndex
             , valueType :: ValueType }
  | SetLocal { index :: BinaryenIndex
             , value :: Expression }
  | TeeLocal { index :: BinaryenIndex
             , value :: Expression }
  | GetGlobal { name :: SBS.ShortByteString
              , valueType :: ValueType }
  | SetGlobal { name :: SBS.ShortByteString
              , value :: Expression }
  | Load { signed :: Bool
         , bytes, offset, align :: BinaryenIndex
         , valueType :: ValueType
         , ptr :: Expression }
  | Store { bytes, offset, align :: BinaryenIndex
          , ptr, value :: Expression
          , valueType :: ValueType }
  | ConstI32 Int32
  | ConstI64 Int64
  | ConstF32 Float
  | ConstF64 Double
  | ConstF32Bits Int32
  | ConstF64Bits Int64
  | Unary { unaryOp :: UnaryOp
          , operand0 :: Expression }
  | Binary { binaryOp :: BinaryOp
           , operand0, operand1 :: Expression }
  | Select { condition, ifTrue, ifFalse :: Expression }
  | Drop { value :: Expression }
  | Return { value :: Expression }
  | Host { hostOp :: HostOp
         , name :: SBS.ShortByteString
         , operands :: V.Vector Expression }
  | Nop
  | Unreachable
  | AtomicRMW { atomicRMWOp :: AtomicRMWOp
              , bytes, offset :: BinaryenIndex
              , ptr, value :: Expression
              , valueType :: ValueType }
  | CFG { graph :: RelooperRun }
  | Unresolved { unresolvedLabel :: SBS.ShortByteString }
  | UnresolvedOff { unresolvedLabel :: SBS.ShortByteString
                  , offset :: BinaryenIndex }
  | UnresolvedGetLocal { unresolvedIndex :: Int
                       , valueType :: ValueType }
  | UnresolvedSetLocal { unresolvedIndex :: Int
                       , value :: Expression }
  | UnresolvedTeeLocal { unresolvedIndex :: Int
                       , value :: Expression }
  | Null
  deriving (Show, Generic, Data)

instance Serialize Expression

instance NFData Expression

data Function = Function
  { functionTypeName :: SBS.ShortByteString
  , varTypes :: V.Vector ValueType
  , body :: Expression
  } deriving (Show, Generic, Data)

instance Serialize Function

instance NFData Function

data FunctionImport = FunctionImport
  { internalName, externalModuleName, externalBaseName, functionTypeName :: SBS.ShortByteString
  } deriving (Show, Generic, Data)

instance Serialize FunctionImport

instance NFData FunctionImport

data TableImport = TableImport
  { internalName, externalModuleName, externalBaseName :: SBS.ShortByteString
  } deriving (Show, Generic, Data)

instance Serialize TableImport

instance NFData TableImport

data GlobalImport = GlobalImport
  { internalName, externalModuleName, externalBaseName :: SBS.ShortByteString
  , globalType :: ValueType
  } deriving (Show, Generic, Data)

instance Serialize GlobalImport

instance NFData GlobalImport

data FunctionExport = FunctionExport
  { internalName, externalName :: SBS.ShortByteString
  } deriving (Show, Generic, Data)

instance Serialize FunctionExport

instance NFData FunctionExport

data TableExport = TableExport
  { internalName, externalName :: SBS.ShortByteString
  } deriving (Show, Generic, Data)

instance Serialize TableExport

instance NFData TableExport

data GlobalExport = GlobalExport
  { internalName, externalName :: SBS.ShortByteString
  } deriving (Show, Generic, Data)

instance Serialize GlobalExport

instance NFData GlobalExport

data Global = Global
  { valueType :: ValueType
  , mutable :: Bool
  , initValue :: Expression
  } deriving (Show, Generic, Data)

instance Serialize Global

instance NFData Global

newtype FunctionTable = FunctionTable
  { functionNames :: V.Vector SBS.ShortByteString
  } deriving (Show, Generic, Data)

instance Serialize FunctionTable

instance NFData FunctionTable

data DataSegment = DataSegment
  { content :: SBS.ShortByteString
  , offset :: Expression
  } deriving (Show, Generic, Data)

instance Serialize DataSegment

instance NFData DataSegment

data Memory = Memory
  { initialPages, maximumPages :: BinaryenIndex
  , exportName :: SBS.ShortByteString
  , dataSegments :: V.Vector DataSegment
  } deriving (Show, Generic, Data)

instance Serialize Memory

instance NFData Memory

data Module = Module
  { functionTypeMap :: HM.HashMap SBS.ShortByteString FunctionType
  , functionMap :: HM.HashMap SBS.ShortByteString Function
  , functionImports :: V.Vector FunctionImport
  , tableImports :: V.Vector TableImport
  , globalImports :: V.Vector GlobalImport
  , functionExports :: V.Vector FunctionExport
  , tableExports :: V.Vector TableExport
  , globalExports :: V.Vector GlobalExport
  , globalMap :: HM.HashMap SBS.ShortByteString Global
  , functionTable :: Maybe FunctionTable
  , memory :: Maybe Memory
  , startFunctionName :: Maybe SBS.ShortByteString
  } deriving (Show, Generic, Data)

instance Serialize Module

instance NFData Module

data RelooperAddBlock
  = AddBlock { code :: Expression }
  | AddBlockWithSwitch { code, condition :: Expression }
  deriving (Show, Generic, Data)

instance Serialize RelooperAddBlock

instance NFData RelooperAddBlock

data RelooperAddBranch
  = AddBranch { to :: SBS.ShortByteString
              , condition, code :: Expression }
  | AddBranchForSwitch { to :: SBS.ShortByteString
                       , indexes :: V.Vector BinaryenIndex
                       , code :: Expression }
  deriving (Show, Generic, Data)

instance Serialize RelooperAddBranch

instance NFData RelooperAddBranch

data RelooperBlock = RelooperBlock
  { addBlock :: RelooperAddBlock
  , addBranches :: V.Vector RelooperAddBranch
  } deriving (Show, Generic, Data)

instance Serialize RelooperBlock

instance NFData RelooperBlock

data RelooperRun = RelooperRun
  { entry :: SBS.ShortByteString
  , blockMap :: HM.HashMap SBS.ShortByteString RelooperBlock
  , labelHelper :: Maybe BinaryenIndex
  } deriving (Show, Generic, Data)

instance Serialize RelooperRun

instance NFData RelooperRun

data MarshalError
  = UnsupportedExpression Expression
  | MissingLabelHelper RelooperRun
  deriving (Show)

instance Exception MarshalError

emptyModule :: Module
emptyModule =
  Module
    { functionTypeMap = []
    , functionMap = []
    , functionImports = []
    , tableImports = []
    , globalImports = []
    , functionExports = []
    , tableExports = []
    , globalExports = []
    , globalMap = []
    , functionTable = Nothing
    , memory = Nothing
    , startFunctionName = Nothing
    }

marshalBool :: Bool -> Int8
marshalBool flag =
  if flag
    then 1
    else 0

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

marshalHostOp :: HostOp -> BinaryenOp
marshalHostOp op =
  case op of
    PageSize -> c_BinaryenPageSize
    CurrentMemory -> c_BinaryenCurrentMemory
    GrowMemory -> c_BinaryenGrowMemory
    HasFeature -> c_BinaryenHasFeature

marshalAtomicRMWOp :: AtomicRMWOp -> BinaryenOp
marshalAtomicRMWOp op =
  case op of
    AtomicRMWAdd -> c_BinaryenAtomicRMWAdd
    AtomicRMWSub -> c_BinaryenAtomicRMWSub
    AtomicRMWAnd -> c_BinaryenAtomicRMWAnd
    AtomicRMWOr -> c_BinaryenAtomicRMWOr
    AtomicRMWXor -> c_BinaryenAtomicRMWXor
    AtomicRMWXchg -> c_BinaryenAtomicRMWXchg

marshalFunctionType ::
     MonadIO m
  => BinaryenModuleRef
  -> SBS.ShortByteString
  -> FunctionType
  -> m BinaryenFunctionTypeRef
marshalFunctionType m k FunctionType {..} =
  liftIO $
  withSV (V.convert $ V.map marshalValueType paramTypes) $ \pts ptl ->
    withSBS k $ \np ->
      c_BinaryenAddFunctionType m np (marshalValueType returnType) pts ptl

marshalExpression ::
     MonadIO m => BinaryenModuleRef -> Expression -> m BinaryenExpressionRef
marshalExpression m e =
  liftIO $
  case e of
    Block {..} -> do
      bs <- fmap V.convert $ V.forM bodys $ marshalExpression m
      withSV bs $ \bsp bl ->
        withSBS name $ \np ->
          c_BinaryenBlock m np bsp bl (marshalValueType valueType)
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
    Call {..} -> do
      os <- fmap V.convert $ V.forM operands $ marshalExpression m
      withSV os $ \ops osl ->
        withSBS target $ \tp ->
          c_BinaryenCall m tp ops osl (marshalValueType valueType)
    CallImport {..} -> do
      os <- fmap V.convert $ V.forM operands $ marshalExpression m
      withSV os $ \ops osl ->
        withSBS target $ \tp ->
          c_BinaryenCallImport m tp ops osl (marshalValueType valueType)
    CallIndirect {..} -> do
      t <- marshalExpression m indirectTarget
      os <- fmap V.convert $ V.forM operands $ marshalExpression m
      withSV os $ \ops osl ->
        withSBS typeName $ \tp -> c_BinaryenCallIndirect m t ops osl tp
    GetLocal {..} -> c_BinaryenGetLocal m index $ marshalValueType valueType
    SetLocal {..} -> do
      v <- marshalExpression m value
      c_BinaryenSetLocal m index v
    TeeLocal {..} -> do
      v <- marshalExpression m value
      c_BinaryenTeeLocal m index v
    GetGlobal {..} ->
      withSBS name $ \np ->
        c_BinaryenGetGlobal m np (marshalValueType valueType)
    SetGlobal {..} -> do
      v <- marshalExpression m value
      withSBS name $ \np -> c_BinaryenSetGlobal m np v
    Load {..} -> do
      p <- marshalExpression m ptr
      c_BinaryenLoad
        m
        bytes
        (marshalBool signed)
        offset
        align
        (marshalValueType valueType)
        p
    Store {..} -> do
      p <- marshalExpression m ptr
      v <- marshalExpression m value
      c_BinaryenStore m bytes offset align p v (marshalValueType valueType)
    ConstI32 x -> c_BinaryenConstInt32 m x
    ConstI64 x -> c_BinaryenConstInt64 m x
    ConstF32 x -> c_BinaryenConstFloat32 m x
    ConstF64 x -> c_BinaryenConstFloat64 m x
    ConstF32Bits x -> c_BinaryenConstFloat32Bits m x
    ConstF64Bits x -> c_BinaryenConstFloat64Bits m x
    Unary {..} -> do
      x <- marshalExpression m operand0
      c_BinaryenUnary m (marshalUnaryOp unaryOp) x
    Binary {..} -> do
      x <- marshalExpression m operand0
      y <- marshalExpression m operand1
      c_BinaryenBinary m (marshalBinaryOp binaryOp) x y
    Select {..} -> do
      c <- marshalExpression m condition
      t <- marshalExpression m ifTrue
      f <- marshalExpression m ifFalse
      c_BinaryenSelect m c t f
    Drop {..} -> do
      v <- marshalExpression m value
      c_BinaryenDrop m v
    Return {..} -> do
      v <- marshalExpression m value
      c_BinaryenReturn m v
    Host {..} -> do
      xs <- fmap V.convert $ V.forM operands $ marshalExpression m
      withSV xs $ \es en ->
        withSBS name $ \np -> c_BinaryenHost m (marshalHostOp hostOp) np es en
    Nop -> c_BinaryenNop m
    Unreachable -> c_BinaryenUnreachable m
    AtomicRMW {..} -> do
      p <- marshalExpression m ptr
      v <- marshalExpression m value
      c_BinaryenAtomicRMW
        m
        (marshalAtomicRMWOp atomicRMWOp)
        bytes
        offset
        p
        v
        (marshalValueType valueType)
    CFG {..} -> relooperRun m graph
    Null -> pure nullPtr
    _ -> throwIO $ UnsupportedExpression e

marshalFunction ::
     MonadIO m
  => BinaryenModuleRef
  -> SBS.ShortByteString
  -> BinaryenFunctionTypeRef
  -> Function
  -> m BinaryenFunctionRef
marshalFunction m k ft Function {..} =
  liftIO $ do
    b <- marshalExpression m body
    withSV (V.convert $ V.map marshalValueType varTypes) $ \vtp vtl ->
      withSBS k $ \np -> c_BinaryenAddFunction m np ft vtp vtl b

marshalFunctionImport ::
     MonadIO m
  => BinaryenModuleRef
  -> BinaryenFunctionTypeRef
  -> FunctionImport
  -> m BinaryenImportRef
marshalFunctionImport m ft FunctionImport {..} =
  liftIO $
  withSBS internalName $ \inp ->
    withSBS externalModuleName $ \emp ->
      withSBS externalBaseName $ \ebp ->
        c_BinaryenAddFunctionImport m inp emp ebp ft

marshalTableImport ::
     MonadIO m => BinaryenModuleRef -> TableImport -> m BinaryenImportRef
marshalTableImport m TableImport {..} =
  liftIO $
  withSBS internalName $ \inp ->
    withSBS externalModuleName $ \emp ->
      withSBS externalBaseName $ \ebp -> c_BinaryenAddTableImport m inp emp ebp

marshalGlobalImport ::
     MonadIO m => BinaryenModuleRef -> GlobalImport -> m BinaryenImportRef
marshalGlobalImport m GlobalImport {..} =
  liftIO $
  withSBS internalName $ \inp ->
    withSBS externalModuleName $ \emp ->
      withSBS externalBaseName $ \ebp ->
        c_BinaryenAddGlobalImport m inp emp ebp (marshalValueType globalType)

marshalFunctionExport ::
     MonadIO m => BinaryenModuleRef -> FunctionExport -> m BinaryenExportRef
marshalFunctionExport m FunctionExport {..} =
  liftIO $
  withSBS internalName $ \inp ->
    withSBS externalName $ \enp -> c_BinaryenAddFunctionExport m inp enp

marshalTableExport ::
     MonadIO m => BinaryenModuleRef -> TableExport -> m BinaryenExportRef
marshalTableExport m TableExport {..} =
  liftIO $
  withSBS internalName $ \inp ->
    withSBS externalName $ \enp -> c_BinaryenAddTableExport m inp enp

marshalGlobalExport ::
     MonadIO m => BinaryenModuleRef -> GlobalExport -> m BinaryenExportRef
marshalGlobalExport m GlobalExport {..} =
  liftIO $
  withSBS internalName $ \inp ->
    withSBS externalName $ \enp -> c_BinaryenAddGlobalExport m inp enp

marshalGlobal ::
     MonadIO m
  => BinaryenModuleRef
  -> SBS.ShortByteString
  -> Global
  -> m BinaryenGlobalRef
marshalGlobal m k Global {..} =
  liftIO $ do
    i <- marshalExpression m initValue
    withSBS k $ \kp ->
      c_BinaryenAddGlobal
        m
        kp
        (marshalValueType valueType)
        (marshalBool mutable)
        i

marshalFunctionTable ::
     MonadIO m
  => BinaryenModuleRef
  -> HM.HashMap SBS.ShortByteString BinaryenFunctionRef
  -> FunctionTable
  -> m ()
marshalFunctionTable m fps FunctionTable {..} =
  liftIO $
  withSV (V.convert $ V.map (fps HM.!) functionNames) $
  c_BinaryenSetFunctionTable m

marshalMemory :: MonadIO m => BinaryenModuleRef -> Memory -> m ()
marshalMemory m Memory {..} =
  liftIO $ do
    (cps, os) <-
      fmap V.unzip $
      V.forM dataSegments $ \DataSegment {..} -> do
        o <- marshalExpression m offset
        withSBS content $ \cp -> pure (cp, o)
    withSV (V.convert cps) $ \cp (_ :: Int) ->
      withSV (V.convert os) $ \ofs (_ :: Int) ->
        withSV
          (V.convert $
           V.map
             (\DataSegment {..} -> fromIntegral $ SBS.length content)
             dataSegments) $ \sps (_ :: Int) ->
          withSBS exportName $ \enp ->
            c_BinaryenSetMemory
              m
              initialPages
              maximumPages
              enp
              cp
              ofs
              sps
              (fromIntegral $ V.length dataSegments)

marshalStartFunctionName ::
     MonadIO m
  => BinaryenModuleRef
  -> HM.HashMap SBS.ShortByteString BinaryenFunctionRef
  -> SBS.ShortByteString
  -> m ()
marshalStartFunctionName m fps n = liftIO $ c_BinaryenSetStart m (fps HM.! n)

marshalModule :: MonadIO m => Module -> m BinaryenModuleRef
marshalModule Module {..} =
  liftIO $ do
    m <- c_BinaryenModuleCreate
    ftps <-
      fmap HM.fromList $
      for (HM.toList functionTypeMap) $ \(k, ft) -> do
        ftp <- marshalFunctionType m k ft
        pure (k, ftp)
    fps <-
      fmap HM.fromList $
      for (HM.toList functionMap) $ \(k, f@Function {..}) -> do
        fp <- marshalFunction m k (ftps HM.! functionTypeName) f
        pure (k, fp)
    V.forM_ functionImports $ \fi@FunctionImport {..} ->
      marshalFunctionImport m (ftps HM.! functionTypeName) fi
    V.forM_ tableImports $ marshalTableImport m
    V.forM_ globalImports $ marshalGlobalImport m
    V.forM_ functionExports $ marshalFunctionExport m
    V.forM_ tableExports $ marshalTableExport m
    V.forM_ globalExports $ marshalGlobalExport m
    for_ (HM.toList globalMap) $ uncurry (marshalGlobal m)
    case functionTable of
      Just ft -> marshalFunctionTable m fps ft
      _ -> pure ()
    case memory of
      Just mem -> marshalMemory m mem
      _ -> pure ()
    case startFunctionName of
      Just k -> marshalStartFunctionName m fps k
      _ -> pure ()
    pure m

relooperAddBlock ::
     MonadIO m
  => BinaryenModuleRef
  -> RelooperRef
  -> RelooperAddBlock
  -> m RelooperBlockRef
relooperAddBlock m r ab =
  liftIO $
  case ab of
    AddBlock {..} -> do
      c <- marshalExpression m code
      c_RelooperAddBlock r c
    AddBlockWithSwitch {..} -> do
      _code <- marshalExpression m code
      _cond <- marshalExpression m condition
      c_RelooperAddBlockWithSwitch r _code _cond

relooperAddBranch ::
     MonadIO m
  => BinaryenModuleRef
  -> HM.HashMap SBS.ShortByteString RelooperBlockRef
  -> SBS.ShortByteString
  -> RelooperAddBranch
  -> m ()
relooperAddBranch m bm k ab =
  liftIO $
  case ab of
    AddBranch {..} -> do
      _cond <- marshalExpression m condition
      _code <- marshalExpression m code
      c_RelooperAddBranch (bm HM.! k) (bm HM.! to) _cond _code
    AddBranchForSwitch {..} -> do
      c <- marshalExpression m code
      withSV (V.convert indexes) $ \idp idn ->
        c_RelooperAddBranchForSwitch (bm HM.! k) (bm HM.! to) idp idn c

relooperRun ::
     MonadIO m => BinaryenModuleRef -> RelooperRun -> m BinaryenExpressionRef
relooperRun m rr@RelooperRun {..} =
  liftIO $ do
    r <- c_RelooperCreate
    bpm <-
      fmap HM.fromList $
      for (HM.toList blockMap) $ \(k, RelooperBlock {..}) -> do
        bp <- relooperAddBlock m r addBlock
        pure (k, bp)
    for_ (HM.toList blockMap) $ \(k, RelooperBlock {..}) ->
      V.forM_ addBranches $ relooperAddBranch m bpm k
    case labelHelper of
      Just lh -> c_RelooperRenderAndDispose r (bpm HM.! entry) lh m
      _ -> throwIO $ MissingLabelHelper rr

instance Serialize SBS.ShortByteString where
  {-# INLINE put #-}
  put sbs = put (SBS.length sbs) *> putShortByteString sbs
  {-# INLINE get #-}
  get = get >>= getShortByteString

instance (Eq k, Hashable k, Serialize k, Serialize v) =>
         Serialize (HM.HashMap k v) where
  {-# INLINE put #-}
  put = put . HM.toList
  {-# INLINE get #-}
  get = HM.fromList <$> get

instance Serialize a => Serialize (V.Vector a) where
  {-# INLINE put #-}
  put v = put (V.length v) *> V.mapM_ put v
  {-# INLINE get #-}
  get = do
    len <- get
    V.replicateM len get
