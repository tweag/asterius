{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Asterius.Types
  ( BinaryenIndex,
    AsteriusCodeGenError (..),
    AsteriusStatic (..),
    AsteriusStaticsType (..),
    AsteriusStatics (..),
    AsteriusModule (..),
    AsteriusModuleSymbol (..),
    AsteriusEntitySymbol (..),
    UnresolvedLocalReg (..),
    UnresolvedGlobalReg (..),
    ValueType (..),
    FunctionType (..),
    UnaryOp (..),
    BinaryOp (..),
    HostOp (..),
    Expression (..),
    Function (..),
    FunctionImport (..),
    TableImport (..),
    MemoryImport (..),
    FunctionExport (..),
    TableExport (..),
    MemoryExport (..),
    FunctionTable (..),
    DataSegment (..),
    Module (..),
    RelooperAddBlock (..),
    RelooperAddBranch (..),
    RelooperBlock (..),
    RelooperRun (..),
    Chunk (..),
    FFIValueType (..),
    FFIFunctionType (..),
    FFISafety (..),
    FFIImportDecl (..),
    FFIExportDecl (..),
    FFIMarshalState (..),
  )
where

import Asterius.Internals.Binary
import Control.Exception
import Data.Binary
import qualified Data.ByteString.Short as SBS
import Data.Data
import qualified Data.Map.Lazy as LM
import Data.String
import Foreign
import GHC.Generics

type BinaryenIndex = Word32

data AsteriusCodeGenError
  = UnsupportedCmmLit SBS.ShortByteString
  | UnsupportedCmmInstr SBS.ShortByteString
  | UnsupportedCmmBranch SBS.ShortByteString
  | UnsupportedCmmType SBS.ShortByteString
  | UnsupportedCmmWidth SBS.ShortByteString
  | UnsupportedCmmGlobalReg SBS.ShortByteString
  | UnsupportedCmmExpr SBS.ShortByteString
  | UnsupportedCmmSectionType SBS.ShortByteString
  | UnsupportedImplicitCasting Expression ValueType ValueType
  | AssignToImmutableGlobalReg UnresolvedGlobalReg
  deriving (Eq, Show, Generic, Data)

instance Binary AsteriusCodeGenError

instance Exception AsteriusCodeGenError

data AsteriusStatic
  = SymbolStatic AsteriusEntitySymbol Int
  | Uninitialized Int
  | Serialized SBS.ShortByteString
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary AsteriusStatic

data AsteriusStaticsType
  = ConstBytes
  | Bytes
  | InfoTable
  | Closure
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary AsteriusStaticsType

data AsteriusStatics
  = AsteriusStatics
      { staticsType :: AsteriusStaticsType,
        asteriusStatics :: [AsteriusStatic]
      }
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary AsteriusStatics

data AsteriusModule
  = AsteriusModule
      { staticsMap :: LM.Map AsteriusEntitySymbol AsteriusStatics,
        staticsErrorMap :: LM.Map AsteriusEntitySymbol AsteriusCodeGenError,
        functionMap :: LM.Map AsteriusEntitySymbol Function,
        sptMap :: LM.Map AsteriusEntitySymbol (Word64, Word64),
        ffiMarshalState :: FFIMarshalState
      }
  deriving (Eq, Show, Generic, Data)

instance Binary AsteriusModule where
  put AsteriusModule {..} =
    lazyMapPut staticsMap
      *> lazyMapPut staticsErrorMap
      *> lazyMapPut functionMap
      *> lazyMapPut sptMap
      *> put ffiMarshalState
  get =
    AsteriusModule
      <$> lazyMapGet
      <*> lazyMapGet
      <*> lazyMapGet
      <*> lazyMapGet
      <*> get

instance Semigroup AsteriusModule where
  AsteriusModule sm0 se0 fm0 spt0 mod_ffi_state0 <> AsteriusModule sm1 se1 fm1 spt1 mod_ffi_state1 =
    AsteriusModule
      (sm0 <> sm1)
      (se0 <> se1)
      (fm0 <> fm1)
      (spt0 <> spt1)
      (mod_ffi_state0 <> mod_ffi_state1)

instance Monoid AsteriusModule where
  mempty = AsteriusModule mempty mempty mempty mempty mempty

data AsteriusModuleSymbol
  = AsteriusModuleSymbol
      { unitId :: SBS.ShortByteString,
        moduleName :: [SBS.ShortByteString]
      }
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary AsteriusModuleSymbol

newtype AsteriusEntitySymbol
  = AsteriusEntitySymbol
      { entityName :: SBS.ShortByteString
      }
  deriving (Eq, Ord, IsString, Binary, Semigroup)

deriving newtype instance Show AsteriusEntitySymbol

deriving instance Data AsteriusEntitySymbol

data UnresolvedLocalReg
  = UniqueLocalReg Int ValueType
  | QuotRemI32X
  | QuotRemI32Y
  | QuotRemI64X
  | QuotRemI64Y
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary UnresolvedLocalReg

data UnresolvedGlobalReg
  = VanillaReg Int
  | FloatReg Int
  | DoubleReg Int
  | LongReg Int
  | Sp
  | SpLim
  | Hp
  | HpLim
  | CCCS
  | CurrentTSO
  | CurrentNursery
  | HpAlloc
  | EagerBlackholeInfo
  | GCEnter1
  | GCFun
  | BaseReg
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary UnresolvedGlobalReg

data ValueType
  = I32
  | I64
  | F32
  | F64
  deriving (Enum, Eq, Ord, Show, Generic, Data)

instance Binary ValueType

data FunctionType
  = FunctionType
      { paramTypes, returnTypes :: [ValueType]
      }
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary FunctionType

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
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary UnaryOp

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
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary BinaryOp

data HostOp
  = CurrentMemory
  | GrowMemory
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary HostOp

data Expression
  = Block
      { name :: SBS.ShortByteString,
        bodys :: [Expression],
        blockReturnTypes :: [ValueType]
      }
  | If
      { condition, ifTrue :: Expression,
        ifFalse :: Maybe Expression
      }
  | Loop
      { name :: SBS.ShortByteString,
        body :: Expression
      }
  | Break
      { name :: SBS.ShortByteString,
        breakCondition :: Maybe Expression
      }
  | Switch
      { names :: [SBS.ShortByteString],
        defaultName :: SBS.ShortByteString,
        condition :: Expression
      }
  | Call
      { target :: AsteriusEntitySymbol,
        operands :: [Expression],
        callReturnTypes :: [ValueType]
      }
  | CallImport
      { target' :: SBS.ShortByteString,
        operands :: [Expression],
        callImportReturnTypes :: [ValueType]
      }
  | CallIndirect
      { indirectTarget :: Expression,
        operands :: [Expression],
        functionType :: FunctionType
      }
  | GetLocal
      { index :: BinaryenIndex,
        valueType :: ValueType
      }
  | SetLocal
      { index :: BinaryenIndex,
        value :: Expression
      }
  | TeeLocal
      { index :: BinaryenIndex,
        value :: Expression,
        valueType :: ValueType
      }
  | Load
      { signed :: Bool,
        bytes, offset :: BinaryenIndex,
        valueType :: ValueType,
        ptr :: Expression
      }
  | Store
      { bytes, offset :: BinaryenIndex,
        ptr, value :: Expression,
        valueType :: ValueType
      }
  | ConstI32 Int32
  | ConstI64 Int64
  | ConstF32 Float
  | ConstF64 Double
  | Unary
      { unaryOp :: UnaryOp,
        operand0 :: Expression
      }
  | Binary
      { binaryOp :: BinaryOp,
        operand0, operand1 :: Expression
      }
  | Drop
      { dropValue :: Expression
      }
  | ReturnCall
      { returnCallTarget64 :: AsteriusEntitySymbol
      }
  | ReturnCallIndirect
      { returnCallIndirectTarget64 :: Expression
      }
  | Host
      { hostOp :: HostOp,
        operands :: [Expression]
      }
  | Nop
  | Unreachable
  | CFG
      { graph :: RelooperRun
      }
  | Symbol
      { unresolvedSymbol :: AsteriusEntitySymbol,
        symbolOffset :: Int
      }
  | UnresolvedGetLocal
      { unresolvedLocalReg :: UnresolvedLocalReg
      }
  | UnresolvedSetLocal
      { unresolvedLocalReg :: UnresolvedLocalReg,
        value :: Expression
      }
  | Barf
      { barfMessage :: SBS.ShortByteString,
        barfReturnTypes :: [ValueType]
      }
  deriving (Eq, Show, Generic, Data)

instance Binary Expression

data Function
  = Function
      { functionType :: FunctionType,
        varTypes :: [ValueType],
        body :: Expression
      }
  deriving (Eq, Show, Generic, Data)

instance Binary Function

data FunctionImport
  = FunctionImport
      { internalName, externalModuleName, externalBaseName :: SBS.ShortByteString,
        functionType :: FunctionType
      }
  deriving (Eq, Show, Data, Generic)

instance Binary FunctionImport

data TableImport
  = TableImport
      { externalModuleName, externalBaseName :: SBS.ShortByteString
      }
  deriving (Eq, Show, Data, Generic)

instance Binary TableImport

data MemoryImport
  = MemoryImport
      { externalModuleName, externalBaseName :: SBS.ShortByteString
      }
  deriving (Eq, Show, Data, Generic)

instance Binary MemoryImport

data FunctionExport
  = FunctionExport
      { internalName, externalName :: SBS.ShortByteString
      }
  deriving (Eq, Show, Data, Generic)

instance Binary FunctionExport

newtype TableExport
  = TableExport
      { externalName :: SBS.ShortByteString
      }
  deriving (Eq, Show, Data, Generic)

instance Binary TableExport

newtype MemoryExport
  = MemoryExport
      { externalName :: SBS.ShortByteString
      }
  deriving (Eq, Show, Data, Generic)

instance Binary MemoryExport

data FunctionTable
  = FunctionTable
      { tableFunctionNames :: [SBS.ShortByteString],
        tableOffset :: BinaryenIndex
      }
  deriving (Eq, Show, Data, Generic)

instance Binary FunctionTable

data DataSegment
  = DataSegment
      { content :: SBS.ShortByteString,
        offset :: Int32
      }
  deriving (Eq, Show, Data, Generic)

instance Binary DataSegment

data Module
  = Module
      { functionMap' :: LM.Map SBS.ShortByteString Function,
        functionImports :: [FunctionImport],
        functionExports :: [FunctionExport],
        functionTable :: FunctionTable,
        tableImport :: TableImport,
        tableExport :: TableExport,
        tableSlots :: Int,
        memorySegments :: [DataSegment],
        memoryImport :: MemoryImport,
        memoryExport :: MemoryExport,
        memoryMBlocks :: Int
      }
  deriving (Eq, Show, Data, Generic)

instance Binary Module

data RelooperAddBlock
  = AddBlock
      { code :: Expression
      }
  | AddBlockWithSwitch
      { code, condition :: Expression
      }
  deriving (Eq, Show, Generic, Data)

instance Binary RelooperAddBlock

data RelooperAddBranch
  = AddBranch
      { to :: SBS.ShortByteString,
        addBranchCondition :: Maybe Expression
      }
  | AddBranchForSwitch
      { to :: SBS.ShortByteString,
        indexes :: [BinaryenIndex]
      }
  deriving (Eq, Show, Generic, Data)

instance Binary RelooperAddBranch

data RelooperBlock
  = RelooperBlock
      { addBlock :: RelooperAddBlock,
        addBranches :: [RelooperAddBranch]
      }
  deriving (Eq, Show, Generic, Data)

instance Binary RelooperBlock

data RelooperRun
  = RelooperRun
      { entry :: SBS.ShortByteString,
        blockMap :: LM.Map SBS.ShortByteString RelooperBlock,
        labelHelper :: BinaryenIndex
      }
  deriving (Eq, Show, Generic, Data)

instance Binary RelooperRun

data Chunk a
  = Lit String
  | Field a
  deriving (Eq, Show, Generic, Data)

instance Binary a => Binary (Chunk a)

data FFIValueType
  = FFI_VAL
      { ffiWasmValueType, ffiJSValueType :: ValueType,
        hsTyCon :: SBS.ShortByteString,
        signed :: Bool
      }
  | FFI_JSVAL
  deriving (Eq, Show, Generic, Data)

instance Binary FFIValueType

data FFIFunctionType
  = FFIFunctionType
      { ffiParamTypes, ffiResultTypes :: [FFIValueType],
        ffiInIO :: Bool
      }
  deriving (Eq, Show, Generic, Data)

instance Binary FFIFunctionType

data FFISafety
  = FFIUnsafe
  | FFISafe
  | FFIInterruptible
  deriving (Eq, Show, Generic, Data)

instance Binary FFISafety

data FFIImportDecl
  = FFIImportDecl
      { ffiFunctionType :: FFIFunctionType,
        ffiSafety :: FFISafety,
        ffiSourceChunks :: [Chunk Int]
      }
  deriving (Eq, Show, Generic, Data)

instance Binary FFIImportDecl

data FFIExportDecl
  = FFIExportDecl
      { ffiFunctionType :: FFIFunctionType,
        ffiExportClosure :: AsteriusEntitySymbol
      }
  deriving (Eq, Show, Generic, Data)

instance Binary FFIExportDecl

data FFIMarshalState
  = FFIMarshalState
      { ffiImportDecls :: LM.Map AsteriusEntitySymbol FFIImportDecl,
        ffiExportDecls :: LM.Map AsteriusEntitySymbol FFIExportDecl
      }
  deriving (Eq, Show, Data)

instance Semigroup FFIMarshalState where
  s0 <> s1 = FFIMarshalState
    { ffiImportDecls = ffiImportDecls s0 <> ffiImportDecls s1,
      ffiExportDecls = ffiExportDecls s0 <> ffiExportDecls s1
    }

instance Monoid FFIMarshalState where
  mempty = FFIMarshalState {ffiImportDecls = mempty, ffiExportDecls = mempty}

instance Binary FFIMarshalState where

  put FFIMarshalState {..} =
    lazyMapPut ffiImportDecls *> lazyMapPut ffiExportDecls

  get = FFIMarshalState <$> lazyMapGet <*> lazyMapGet
