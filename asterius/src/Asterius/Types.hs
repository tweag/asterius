{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Types
  ( BinaryenIndex,
    AsteriusCodeGenError (..),
    AsteriusStatic (..),
    AsteriusStaticsType (..),
    AsteriusStatics (..),
    AsteriusModule (..),
    AsteriusModuleSymbol (..),
    EntitySymbol,
    entityName,
    mkEntitySymbol,
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
    FFIValueTypeRep (..),
    FFIValueType (..),
    FFIFunctionType (..),
    FFISafety (..),
    FFIImportDecl (..),
    FFIExportDecl (..),
    FFIMarshalState (..),
  )
where

import Asterius.Binary.Generic
import Asterius.Binary.Orphans ()
import Asterius.Types.EntitySymbol
import qualified Binary as GHC
import Control.Exception
import qualified Data.ByteString as BS
import Data.Data
import qualified Data.Map.Lazy as LM
import Foreign
import GHC.Generics

type BinaryenIndex = Word32

data AsteriusCodeGenError
  = UnsupportedCmmLit BS.ByteString
  | UnsupportedCmmInstr BS.ByteString
  | UnsupportedCmmBranch BS.ByteString
  | UnsupportedCmmType BS.ByteString
  | UnsupportedCmmWidth BS.ByteString
  | UnsupportedCmmGlobalReg BS.ByteString
  | UnsupportedCmmExpr BS.ByteString
  | UnsupportedCmmSectionType BS.ByteString
  | UnsupportedImplicitCasting Expression ValueType ValueType
  | AssignToImmutableGlobalReg UnresolvedGlobalReg
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary AsteriusCodeGenError where
  put_ = gPut_
  get = gGet

instance Exception AsteriusCodeGenError

data AsteriusStatic
  = SymbolStatic EntitySymbol Int
  | Uninitialized Int
  | Serialized BS.ByteString
  deriving (Eq, Ord, Show, Generic, Data)

instance GHC.Binary AsteriusStatic where
  put_ = gPut_
  get = gGet

data AsteriusStaticsType
  = ConstBytes
  | Bytes
  | InfoTable
  | Closure
  deriving (Eq, Ord, Show, Generic, Data)

instance GHC.Binary AsteriusStaticsType where
  put_ = gPut_
  get = gGet

data AsteriusStatics
  = AsteriusStatics
      { staticsType :: AsteriusStaticsType,
        asteriusStatics :: [AsteriusStatic]
      }
  deriving (Eq, Ord, Show, Generic, Data)

instance GHC.Binary AsteriusStatics where
  put_ = gPut_
  get = gGet

data AsteriusModule
  = AsteriusModule
      { staticsMap :: LM.Map EntitySymbol AsteriusStatics,
        staticsErrorMap :: LM.Map EntitySymbol AsteriusCodeGenError,
        functionMap :: LM.Map EntitySymbol Function,
        sptMap :: LM.Map EntitySymbol (Word64, Word64),
        ffiMarshalState :: FFIMarshalState
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary AsteriusModule where
  put_ = gPut_
  get = gGet

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
      { unitId :: BS.ByteString,
        moduleName :: [BS.ByteString]
      }
  deriving (Eq, Ord, Show, Generic, Data)

instance GHC.Binary AsteriusModuleSymbol where
  put_ = gPut_
  get = gGet

data UnresolvedLocalReg
  = UniqueLocalReg Int ValueType
  | QuotRemI32X
  | QuotRemI32Y
  | QuotRemI64X
  | QuotRemI64Y
  deriving (Eq, Ord, Show, Generic, Data)

instance GHC.Binary UnresolvedLocalReg where
  put_ = gPut_
  get = gGet

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

instance GHC.Binary UnresolvedGlobalReg where
  put_ = gPut_
  get = gGet

data ValueType
  = I32
  | I64
  | F32
  | F64
  deriving (Enum, Eq, Ord, Show, Generic, Data)

instance GHC.Binary ValueType where
  put_ = gPut_
  get = gGet

data FunctionType
  = FunctionType
      { paramTypes, returnTypes :: [ValueType]
      }
  deriving (Eq, Ord, Show, Generic, Data)

instance GHC.Binary FunctionType where
  put_ = gPut_
  get = gGet

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

instance GHC.Binary UnaryOp where
  put_ = gPut_
  get = gGet

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

instance GHC.Binary BinaryOp where
  put_ = gPut_
  get = gGet

data HostOp
  = CurrentMemory
  | GrowMemory
  deriving (Eq, Ord, Show, Generic, Data)

instance GHC.Binary HostOp where
  put_ = gPut_
  get = gGet

data Expression
  = Block
      { name :: BS.ByteString,
        bodys :: [Expression],
        blockReturnTypes :: [ValueType]
      }
  | If
      { condition, ifTrue :: Expression,
        ifFalse :: Maybe Expression
      }
  | Loop
      { name :: BS.ByteString,
        body :: Expression
      }
  | Break
      { name :: BS.ByteString,
        breakCondition :: Maybe Expression
      }
  | Switch
      { names :: [BS.ByteString],
        defaultName :: BS.ByteString,
        condition :: Expression
      }
  | Call
      { target :: EntitySymbol,
        operands :: [Expression],
        callReturnTypes :: [ValueType]
      }
  | CallImport
      { target' :: BS.ByteString,
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
      { returnCallTarget64 :: EntitySymbol
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
      { unresolvedSymbol :: EntitySymbol,
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
      { barfMessage :: BS.ByteString,
        barfReturnTypes :: [ValueType]
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary Expression where
  put_ = gPut_
  get = gGet

data Function
  = Function
      { functionType :: FunctionType,
        varTypes :: [ValueType],
        body :: Expression
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary Function where
  put_ = gPut_
  get = gGet

data FunctionImport
  = FunctionImport
      { internalName, externalModuleName, externalBaseName :: BS.ByteString,
        functionType :: FunctionType
      }
  deriving (Eq, Show, Data, Generic)

instance GHC.Binary FunctionImport where
  put_ = gPut_
  get = gGet

data TableImport
  = TableImport
      { externalModuleName, externalBaseName :: BS.ByteString
      }
  deriving (Eq, Show, Data, Generic)

instance GHC.Binary TableImport where
  put_ = gPut_
  get = gGet

data MemoryImport
  = MemoryImport
      { externalModuleName, externalBaseName :: BS.ByteString
      }
  deriving (Eq, Show, Data, Generic)

instance GHC.Binary MemoryImport where
  put_ = gPut_
  get = gGet

data FunctionExport
  = FunctionExport
      { internalName, externalName :: BS.ByteString
      }
  deriving (Eq, Show, Data, Generic)

instance GHC.Binary FunctionExport where
  put_ = gPut_
  get = gGet

newtype TableExport
  = TableExport
      { externalName :: BS.ByteString
      }
  deriving (Eq, Show, Data, Generic)

instance GHC.Binary TableExport where
  put_ = gPut_
  get = gGet

newtype MemoryExport
  = MemoryExport
      { externalName :: BS.ByteString
      }
  deriving (Eq, Show, Data, Generic)

instance GHC.Binary MemoryExport where
  put_ = gPut_
  get = gGet

data FunctionTable
  = FunctionTable
      { tableFunctionNames :: [BS.ByteString],
        tableOffset :: BinaryenIndex
      }
  deriving (Eq, Show, Data, Generic)

instance GHC.Binary FunctionTable where
  put_ = gPut_
  get = gGet

data DataSegment
  = DataSegment
      { content :: BS.ByteString,
        offset :: Int32
      }
  deriving (Eq, Show, Data, Generic)

instance GHC.Binary DataSegment where
  put_ = gPut_
  get = gGet

data Module
  = Module
      { functionMap' :: LM.Map BS.ByteString Function,
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

instance GHC.Binary Module where
  put_ = gPut_
  get = gGet

data RelooperAddBlock
  = AddBlock
      { code :: Expression
      }
  | AddBlockWithSwitch
      { code, condition :: Expression
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary RelooperAddBlock where
  put_ = gPut_
  get = gGet

data RelooperAddBranch
  = AddBranch
      { to :: BS.ByteString,
        addBranchCondition :: Maybe Expression
      }
  | AddBranchForSwitch
      { to :: BS.ByteString,
        indexes :: [BinaryenIndex]
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary RelooperAddBranch where
  put_ = gPut_
  get = gGet

data RelooperBlock
  = RelooperBlock
      { addBlock :: RelooperAddBlock,
        addBranches :: [RelooperAddBranch]
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary RelooperBlock where
  put_ = gPut_
  get = gGet

data RelooperRun
  = RelooperRun
      { entry :: BS.ByteString,
        blockMap :: LM.Map BS.ByteString RelooperBlock,
        labelHelper :: BinaryenIndex
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary RelooperRun where
  put_ = gPut_
  get = gGet

data FFIValueTypeRep
  = FFILiftedRep
  | FFIUnliftedRep
  | FFIJSValRep
  | FFIIntRep
  | FFIWordRep
  | FFIAddrRep
  | FFIFloatRep
  | FFIDoubleRep
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary FFIValueTypeRep where
  put_ = gPut_
  get = gGet

data FFIValueType
  = FFIValueType
      { ffiValueTypeRep :: FFIValueTypeRep,
        hsTyCon :: BS.ByteString
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary FFIValueType where
  put_ = gPut_
  get = gGet

data FFIFunctionType
  = FFIFunctionType
      { ffiParamTypes, ffiResultTypes :: [FFIValueType],
        ffiInIO :: Bool
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary FFIFunctionType where
  put_ = gPut_
  get = gGet

data FFISafety
  = FFIUnsafe
  | FFISafe
  | FFIInterruptible
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary FFISafety where
  put_ = gPut_
  get = gGet

data FFIImportDecl
  = FFIImportDecl
      { ffiFunctionType :: FFIFunctionType,
        ffiSafety :: FFISafety,
        ffiSourceText :: BS.ByteString
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary FFIImportDecl where
  put_ = gPut_
  get = gGet

data FFIExportDecl
  = FFIExportDecl
      { ffiFunctionType :: FFIFunctionType,
        ffiExportClosure :: EntitySymbol
      }
  deriving (Eq, Show, Generic, Data)

instance GHC.Binary FFIExportDecl where
  put_ = gPut_
  get = gGet

data FFIMarshalState
  = FFIMarshalState
      { ffiImportDecls :: LM.Map EntitySymbol FFIImportDecl,
        ffiExportDecls :: LM.Map EntitySymbol FFIExportDecl
      }
  deriving (Eq, Show, Generic, Data)

instance Semigroup FFIMarshalState where
  s0 <> s1 =
    FFIMarshalState
      { ffiImportDecls = ffiImportDecls s0 <> ffiImportDecls s1,
        ffiExportDecls = ffiExportDecls s0 <> ffiExportDecls s1
      }

instance Monoid FFIMarshalState where
  mempty = FFIMarshalState {ffiImportDecls = mempty, ffiExportDecls = mempty}

instance GHC.Binary FFIMarshalState where
  put_ = gPut_
  get = gGet
