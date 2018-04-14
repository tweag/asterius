{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.WebAssembly.Types
  ( BinaryenIndex
  , UnresolvedSymbol(..)
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
  , emptyModule
  , RelooperAddBlock(..)
  , RelooperAddBranch(..)
  , RelooperBlock(..)
  , RelooperRun(..)
  ) where

import Bindings.Binaryen.Raw hiding (RelooperBlock)
import Control.DeepSeq
import qualified Data.ByteString.Short as SBS
import Data.Data (Data)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Serialize
import Data.String
import qualified Data.Vector as V
import GHC.Generics (Generic)
import UnliftIO.Foreign

newtype UnresolvedSymbol = UnresolvedSymbol SBS.ShortByteString
  deriving stock (Generic, Data)
  deriving newtype (Eq, Show, Serialize, Hashable, NFData, IsString)

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
  | AtomicLoad { bytes, offset :: BinaryenIndex
               , valueType :: ValueType
               , ptr :: Expression }
  | AtomicStore { bytes, offset :: BinaryenIndex
                , ptr, value :: Expression
                , valueType :: ValueType }
  | AtomicRMW { atomicRMWOp :: AtomicRMWOp
              , bytes, offset :: BinaryenIndex
              , ptr, value :: Expression
              , valueType :: ValueType }
  | CFG { graph :: RelooperRun }
  | AtomicCmpxchg { bytes, offset :: BinaryenIndex
                  , ptr, expected, replacement :: Expression
                  , valueType :: ValueType }
  | Unresolved { unresolvedSymbol :: UnresolvedSymbol }
  | UnresolvedOff { unresolvedSymbol :: UnresolvedSymbol
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

instance (Eq k, Hashable k, Serialize k) => Serialize (HS.HashSet k) where
  {-# INLINE put #-}
  put = put . HS.toList
  {-# INLINE get #-}
  get = HS.fromList <$> get

instance Serialize a => Serialize (V.Vector a) where
  {-# INLINE put #-}
  put v = put (V.length v) *> V.mapM_ put v
  {-# INLINE get #-}
  get = do
    len <- get
    V.replicateM len get
