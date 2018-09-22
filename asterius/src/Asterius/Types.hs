{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Asterius.Types
  ( BinaryenIndex
  , AsteriusCodeGenError(..)
  , AsteriusStatic(..)
  , AsteriusStatics(..)
  , AsteriusFunction(..)
  , AsteriusFunctionImport(..)
  , AsteriusModule(..)
  , AsteriusModuleSymbol(..)
  , AsteriusEntitySymbol(..)
  , AsteriusStore(..)
  , UnresolvedLocalReg(..)
  , UnresolvedGlobalReg(..)
  , ErrorMessage(..)
  , ValueType(..)
  , FunctionType(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , HostOp(..)
  , Expression(..)
  , Function(..)
  , FunctionImport(..)
  , FunctionExport(..)
  , FunctionTable(..)
  , DataSegment(..)
  , Memory(..)
  , Module(..)
  , RelooperAddBlock(..)
  , RelooperAddBranch(..)
  , RelooperBlock(..)
  , RelooperRun(..)
  , Chunk(..)
  , FFIValueType(..)
  , FFIFunctionType(..)
  , FFIImportDecl(..)
  , FFIExportDecl(..)
  , FFIMarshalState(..)
  ) where

import Bindings.Binaryen.Raw hiding (RelooperBlock)
import Control.Exception
import Data.Binary
import qualified Data.ByteString.Short as SBS
import Data.Data
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Lazy as LM
import qualified Data.Map.Strict as M
import Data.String
import Foreign
import GHC.Generics

data AsteriusCodeGenError
  = UnsupportedCmmLit SBS.ShortByteString
  | UnsupportedCmmInstr SBS.ShortByteString
  | UnsupportedCmmBranch SBS.ShortByteString
  | UnsupportedCmmType SBS.ShortByteString
  | UnsupportedCmmWidth SBS.ShortByteString
  | UnsupportedCmmGlobalReg SBS.ShortByteString
  | UnsupportedCmmExpr SBS.ShortByteString
  | UnsupportedImplicitCasting Expression
                               ValueType
                               ValueType
  | AssignToImmutableGlobalReg UnresolvedGlobalReg
  deriving (Eq, Show, Generic, Data)

instance Binary AsteriusCodeGenError

instance Exception AsteriusCodeGenError

data AsteriusStatic
  = UnresolvedStatic AsteriusEntitySymbol
  | UnresolvedOffStatic AsteriusEntitySymbol
                        Int
  | Uninitialized Int
  | Serialized SBS.ShortByteString
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary AsteriusStatic

newtype AsteriusStatics = AsteriusStatics
  { asteriusStatics :: [AsteriusStatic]
  } deriving (Eq, Ord, Show, Generic, Data)

instance Binary AsteriusStatics

data AsteriusFunction = AsteriusFunction
  { functionType :: FunctionType
  , body :: Expression
  } deriving (Eq, Show, Generic, Data)

instance Binary AsteriusFunction

data AsteriusFunctionImport = AsteriusFunctionImport
  { internalName, externalModuleName, externalBaseName :: SBS.ShortByteString
  , functionType :: FunctionType
  } deriving (Eq, Show, Generic, Data)

instance Binary AsteriusFunctionImport

data AsteriusModule = AsteriusModule
  { staticsMap :: M.Map AsteriusEntitySymbol AsteriusStatics
  , staticsErrorMap :: M.Map AsteriusEntitySymbol AsteriusCodeGenError
  , functionMap :: M.Map AsteriusEntitySymbol AsteriusFunction
  , functionErrorMap :: M.Map AsteriusEntitySymbol AsteriusCodeGenError
  , ffiMarshalState :: FFIMarshalState
  } deriving (Eq, Show, Generic, Data)

instance Binary AsteriusModule

instance Semigroup AsteriusModule where
  AsteriusModule sm0 se0 fm0 fe0 mod_ffi_state0 <> AsteriusModule sm1 se1 fm1 fe1 mod_ffi_state1 =
    AsteriusModule
      (sm0 <> sm1)
      (se0 <> se1)
      (fm0 <> fm1)
      (fe0 <> fe1)
      (mod_ffi_state0 <> mod_ffi_state1)

instance Monoid AsteriusModule where
  mempty = AsteriusModule mempty mempty mempty mempty mempty

data AsteriusModuleSymbol = AsteriusModuleSymbol
  { unitId :: SBS.ShortByteString
  , moduleName :: [SBS.ShortByteString]
  } deriving (Eq, Ord, Show, Generic, Data)

instance Binary AsteriusModuleSymbol

newtype AsteriusEntitySymbol = AsteriusEntitySymbol
  { entityName :: SBS.ShortByteString
  } deriving (Eq, Ord, IsString, Binary)

deriving newtype instance Show AsteriusEntitySymbol

deriving instance Data AsteriusEntitySymbol

data AsteriusStore = AsteriusStore
  { symbolMap :: M.Map AsteriusEntitySymbol AsteriusModuleSymbol
  , moduleMap :: LM.Map AsteriusModuleSymbol AsteriusModule
  } deriving (Eq, Show, Generic, Data)

instance Semigroup AsteriusStore where
  s0 <> s1 =
    AsteriusStore
      { symbolMap = symbolMap s0 <> symbolMap s1
      , moduleMap = moduleMap s0 <> moduleMap s1
      }

instance Monoid AsteriusStore where
  mempty = AsteriusStore {symbolMap = mempty, moduleMap = mempty}

data UnresolvedLocalReg
  = UniqueLocalReg Int
                   ValueType
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

newtype ErrorMessage = ErrorMessage
  { unErrorMessage :: SBS.ShortByteString
  } deriving (Eq, Ord, Generic, Data)

deriving newtype instance Show ErrorMessage

instance Binary ErrorMessage

data ValueType
  = None
  | I32
  | I64
  | F32
  | F64
  deriving (Eq, Ord, Show, Generic, Data)

instance Binary ValueType

data FunctionType = FunctionType
  { returnType :: ValueType
  , paramTypes :: [ValueType]
  } deriving (Eq, Ord, Show, Generic, Data)

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
  = Block { name :: SBS.ShortByteString
          , bodys :: [Expression]
          , valueType :: ValueType }
  | If { condition, ifTrue, ifFalse :: Expression }
  | Loop { name :: SBS.ShortByteString
         , body :: Expression }
  | Break { name :: SBS.ShortByteString
          , condition :: Expression }
  | Switch { names :: [SBS.ShortByteString]
           , defaultName :: SBS.ShortByteString
           , condition :: Expression }
  | Call { target :: AsteriusEntitySymbol
         , operands :: [Expression]
         , valueType :: ValueType }
  | CallImport { target' :: SBS.ShortByteString
               , operands :: [Expression]
               , valueType :: ValueType }
  | CallIndirect { indirectTarget :: Expression
                 , operands :: [Expression]
                 , typeName :: SBS.ShortByteString }
  | GetLocal { index :: BinaryenIndex
             , valueType :: ValueType }
  | SetLocal { index :: BinaryenIndex
             , value :: Expression }
  | Load { signed :: Bool
         , bytes, offset :: BinaryenIndex
         , valueType :: ValueType
         , ptr :: Expression }
  | Store { bytes, offset :: BinaryenIndex
          , ptr, value :: Expression
          , valueType :: ValueType }
  | ConstI32 Int32
  | ConstI64 Int64
  | ConstF32 Float
  | ConstF64 Double
  | Unary { unaryOp :: UnaryOp
          , operand0 :: Expression }
  | Binary { binaryOp :: BinaryOp
           , operand0, operand1 :: Expression }
  | Host { hostOp :: HostOp
         , name :: SBS.ShortByteString
         , operands :: [Expression] }
  | Nop
  | Unreachable
  | CFG { graph :: RelooperRun }
  | Unresolved { unresolvedSymbol :: AsteriusEntitySymbol }
  | UnresolvedOff { unresolvedSymbol :: AsteriusEntitySymbol
                  , offset' :: Int }
  | UnresolvedGetLocal { unresolvedLocalReg :: UnresolvedLocalReg }
  | UnresolvedSetLocal { unresolvedLocalReg :: UnresolvedLocalReg
                       , value :: Expression }
  | UnresolvedGetGlobal { unresolvedGlobalReg :: UnresolvedGlobalReg }
  | UnresolvedSetGlobal { unresolvedGlobalReg :: UnresolvedGlobalReg
                        , value :: Expression }
  | EmitErrorMessage { errorMessage :: ErrorMessage
                     , valueType :: ValueType }
  | Null
  deriving (Eq, Show, Generic, Data)

instance Binary Expression

data Function = Function
  { functionTypeName :: SBS.ShortByteString
  , varTypes :: [ValueType]
  , body :: Expression
  } deriving (Eq, Show, Generic, Data)

instance Binary Function

data FunctionImport = FunctionImport
  { internalName, externalModuleName, externalBaseName, functionTypeName :: SBS.ShortByteString
  } deriving (Eq, Show, Data, Generic)

instance Binary FunctionImport

data FunctionExport = FunctionExport
  { internalName, externalName :: SBS.ShortByteString
  } deriving (Eq, Show, Data, Generic)

instance Binary FunctionExport

newtype FunctionTable = FunctionTable
  { functionNames :: [SBS.ShortByteString]
  } deriving (Eq, Show, Data, Generic)

instance Binary FunctionTable

data DataSegment = DataSegment
  { content :: SBS.ShortByteString
  , offset :: Expression
  } deriving (Eq, Show, Data, Generic)

instance Binary DataSegment

data Memory = Memory
  { initialPages, maximumPages :: BinaryenIndex
  , exportName :: SBS.ShortByteString
  , dataSegments :: [DataSegment]
  } deriving (Eq, Show, Data, Generic)

instance Binary Memory

data Module = Module
  { functionTypeMap :: M.Map SBS.ShortByteString FunctionType
  , functionMap' :: M.Map SBS.ShortByteString Function
  , functionImports :: [FunctionImport]
  , functionExports :: [FunctionExport]
  , functionTable :: FunctionTable
  , memory :: Memory
  } deriving (Eq, Show, Data, Generic)

instance Binary Module

data RelooperAddBlock
  = AddBlock { code :: Expression }
  | AddBlockWithSwitch { code, condition :: Expression }
  deriving (Eq, Show, Generic, Data)

instance Binary RelooperAddBlock

data RelooperAddBranch
  = AddBranch { to :: SBS.ShortByteString
              , condition, code :: Expression }
  | AddBranchForSwitch { to :: SBS.ShortByteString
                       , indexes :: [BinaryenIndex]
                       , code :: Expression }
  deriving (Eq, Show, Generic, Data)

instance Binary RelooperAddBranch

data RelooperBlock = RelooperBlock
  { addBlock :: RelooperAddBlock
  , addBranches :: [RelooperAddBranch]
  } deriving (Eq, Show, Generic, Data)

instance Binary RelooperBlock

data RelooperRun = RelooperRun
  { entry :: SBS.ShortByteString
  , blockMap :: M.Map SBS.ShortByteString RelooperBlock
  , labelHelper :: BinaryenIndex
  } deriving (Eq, Show, Generic, Data)

instance Binary RelooperRun

data Chunk a
  = Lit String
  | Field a
  deriving (Eq, Show, Generic, Data)

instance Binary a => Binary (Chunk a)

data FFIValueType
  = FFI_VAL { ffiWasmValueType, ffiJSValueType :: ValueType
            , hsTyCon :: SBS.ShortByteString
            , signed :: Bool }
  | FFI_JSREF
  deriving (Eq, Show, Generic, Data)

instance Binary FFIValueType

data FFIFunctionType = FFIFunctionType
  { ffiParamTypes :: [FFIValueType]
  , ffiResultType :: Maybe FFIValueType
  , ffiInIO :: Bool
  } deriving (Eq, Show, Generic, Data)

instance Binary FFIFunctionType

data FFIImportDecl = FFIImportDecl
  { ffiFunctionType :: FFIFunctionType
  , ffiSourceChunks :: [Chunk Int]
  } deriving (Eq, Show, Generic, Data)

instance Binary FFIImportDecl

data FFIExportDecl = FFIExportDecl
  { ffiFunctionType :: FFIFunctionType
  , ffiExportClosure :: AsteriusEntitySymbol
  } deriving (Eq, Show, Generic, Data)

instance Binary FFIExportDecl

data FFIMarshalState = FFIMarshalState
  { ffiImportDecls :: M.Map AsteriusModuleSymbol (IM.IntMap FFIImportDecl)
  , ffiExportDecls :: M.Map AsteriusModuleSymbol (M.Map AsteriusEntitySymbol FFIExportDecl)
  } deriving (Eq, Show, Generic, Data)

instance Semigroup FFIMarshalState where
  s0 <> s1 =
    FFIMarshalState
      { ffiImportDecls = ffiImportDecls s0 <> ffiImportDecls s1
      , ffiExportDecls = ffiExportDecls s0 <> ffiExportDecls s1
      }

instance Monoid FFIMarshalState where
  mempty = FFIMarshalState {ffiImportDecls = mempty, ffiExportDecls = mempty}

instance Binary FFIMarshalState
