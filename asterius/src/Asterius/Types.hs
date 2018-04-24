{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Asterius.Types
  ( BinaryenIndex
  , AsteriusCodeGenError(..)
  , AsteriusStatic(..)
  , AsteriusStatics(..)
  , AsteriusFunction(..)
  , AsteriusModule(..)
  , AsteriusModuleSymbol(..)
  , AsteriusEntityKind(..)
  , AsteriusEntitySymbol(..)
  , UnresolvedLocalReg(..)
  , ValueType(..)
  , FunctionType(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , HostOp(..)
  , AtomicRMWOp(..)
  , Expression(..)
  , AsteriusExpression
  , UnresolvedExpression(..)
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
import qualified Data.ByteString.Short as SBS
import Data.Data
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Serialize
import qualified Data.Vector as V
import Data.Void
import GHC.Generics
import UnliftIO
import UnliftIO.Foreign

data AsteriusCodeGenError
  = UnsupportedCmmLit SBS.ShortByteString
  | UnsupportedCmmInstr SBS.ShortByteString
  | UnsupportedCmmBranch SBS.ShortByteString
  | UnsupportedCmmType SBS.ShortByteString
  | UnsupportedCmmWidth SBS.ShortByteString
  | UnsupportedCmmGlobalReg SBS.ShortByteString
  | UnsupportedCmmExpr SBS.ShortByteString
  | UnsupportedRelooperAddBlock (RelooperAddBlock UnresolvedExpression)
  | UnsupportedImplicitCasting AsteriusExpression
                               ValueType
                               ValueType
  | UnhandledException SBS.ShortByteString
  deriving (Show, Generic, Data)

instance Serialize AsteriusCodeGenError

instance Exception AsteriusCodeGenError

data AsteriusStatic
  = UnresolvedStatic AsteriusEntitySymbol
  | UnresolvedOffStatic AsteriusEntitySymbol
                        Int
  | Uninitialized Int
  | Serialized SBS.ShortByteString
  deriving (Show, Generic, Data)

instance Serialize AsteriusStatic

newtype AsteriusStatics = AsteriusStatics
  { asteriusStatics :: V.Vector AsteriusStatic
  } deriving (Show, Generic, Data)

instance Serialize AsteriusStatics

newtype AsteriusFunction = AsteriusFunction
  { body :: RelooperRun UnresolvedExpression
  } deriving (Show, Generic, Data)

instance Serialize AsteriusFunction

data AsteriusModule = AsteriusModule
  { staticsMap :: HM.HashMap AsteriusEntitySymbol AsteriusStatics
  , staticsErrorMap :: HM.HashMap AsteriusEntitySymbol AsteriusCodeGenError
  , functionMap :: HM.HashMap AsteriusEntitySymbol AsteriusFunction
  , functionErrorMap :: HM.HashMap AsteriusEntitySymbol AsteriusCodeGenError
  } deriving (Show, Generic, Data)

instance Serialize AsteriusModule

instance Semigroup AsteriusModule where
  AsteriusModule sm0 se0 fm0 fe0 <> AsteriusModule sm1 se1 fm1 fe1 =
    AsteriusModule (sm0 <> sm1) (se0 <> se1) (fm0 <> fm1) (fe0 <> fe1)

instance Monoid AsteriusModule where
  mempty = AsteriusModule mempty mempty mempty mempty

data AsteriusModuleSymbol = AsteriusModuleSymbol
  { unitId :: SBS.ShortByteString
  , moduleName :: V.Vector SBS.ShortByteString
  } deriving (Eq, Show, Generic, Data)

instance Serialize AsteriusModuleSymbol

instance Hashable AsteriusModuleSymbol

data AsteriusEntityKind
  = StaticsEntity
  | FunctionEntity
  deriving (Eq, Show, Generic, Data)

instance Serialize AsteriusEntityKind

instance Hashable AsteriusEntityKind

data AsteriusEntitySymbol = AsteriusEntitySymbol
  { entityKind :: AsteriusEntityKind
  , entityName :: SBS.ShortByteString
  } deriving (Eq, Show, Generic, Data)

instance Serialize AsteriusEntitySymbol

instance Hashable AsteriusEntitySymbol

data UnresolvedLocalReg
  = UniqueLocalReg Int
  | RelooperHelperReg
  | SwitchCondReg
  | NarrowStoreReg
  | QuotRemIntOperand0
  | QuotRemIntOperand1
  | QuotRemWordOperand0
  | QuotRemWordOperand1
  deriving (Show, Generic, Data)

instance Serialize UnresolvedLocalReg

data ValueType
  = None
  | I32
  | I64
  | F32
  | F64
  | Auto
  deriving (Eq, Show, Generic, Data)

instance Serialize ValueType

data FunctionType = FunctionType
  { returnType :: ValueType
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

data HostOp
  = PageSize
  | CurrentMemory
  | GrowMemory
  | HasFeature
  deriving (Show, Generic, Data)

instance Serialize HostOp

data AtomicRMWOp
  = AtomicRMWAdd
  | AtomicRMWSub
  | AtomicRMWAnd
  | AtomicRMWOr
  | AtomicRMWXor
  | AtomicRMWXchg
  deriving (Show, Generic, Data)

instance Serialize AtomicRMWOp

data Expression a
  = Block { name :: SBS.ShortByteString
          , bodys :: V.Vector (Expression a)
          , valueType :: ValueType }
  | If { condition, ifTrue, ifFalse :: Expression a }
  | Loop { name :: SBS.ShortByteString
         , body :: Expression a }
  | Break { name :: SBS.ShortByteString
          , condition, value :: Expression a }
  | Switch { names :: V.Vector SBS.ShortByteString
           , defaultName :: SBS.ShortByteString
           , condition, value :: Expression a }
  | Call { target :: AsteriusEntitySymbol
         , operands :: V.Vector (Expression a)
         , valueType :: ValueType }
  | CallImport { target :: AsteriusEntitySymbol
               , operands :: V.Vector (Expression a)
               , valueType :: ValueType }
  | CallIndirect { indirectTarget :: Expression a
                 , operands :: V.Vector (Expression a)
                 , typeName :: SBS.ShortByteString }
  | GetLocal { index :: BinaryenIndex
             , valueType :: ValueType }
  | SetLocal { index :: BinaryenIndex
             , value :: Expression a }
  | TeeLocal { index :: BinaryenIndex
             , value :: Expression a }
  | GetGlobal { name :: SBS.ShortByteString
              , valueType :: ValueType }
  | SetGlobal { name :: SBS.ShortByteString
              , value :: Expression a }
  | Load { signed :: Bool
         , bytes, offset, align :: BinaryenIndex
         , valueType :: ValueType
         , ptr :: Expression a }
  | Store { bytes, offset, align :: BinaryenIndex
          , ptr, value :: Expression a
          , valueType :: ValueType }
  | ConstI32 Int32
  | ConstI64 Int64
  | ConstF32 Float
  | ConstF64 Double
  | ConstF32Bits Int32
  | ConstF64Bits Int64
  | Unary { unaryOp :: UnaryOp
          , operand0 :: Expression a }
  | Binary { binaryOp :: BinaryOp
           , operand0, operand1 :: Expression a }
  | Select { condition, ifTrue, ifFalse :: Expression a }
  | Drop { value :: Expression a }
  | Return { value :: Expression a }
  | Host { hostOp :: HostOp
         , name :: SBS.ShortByteString
         , operands :: V.Vector (Expression a) }
  | Nop
  | Unreachable
  | AtomicLoad { bytes, offset :: BinaryenIndex
               , valueType :: ValueType
               , ptr :: Expression a }
  | AtomicStore { bytes, offset :: BinaryenIndex
                , ptr, value :: Expression a
                , valueType :: ValueType }
  | AtomicRMW { atomicRMWOp :: AtomicRMWOp
              , bytes, offset :: BinaryenIndex
              , ptr, value :: Expression a
              , valueType :: ValueType }
  | AtomicCmpxchg { bytes, offset :: BinaryenIndex
                  , ptr, expected, replacement :: Expression a
                  , valueType :: ValueType }
  | CFG { graph :: RelooperRun a }
  | ExtraExpression a
  | Null
  deriving (Show, Generic, Data)

instance Serialize a => Serialize (Expression a)

data UnresolvedExpression
  = Unresolved { unresolvedSymbol :: AsteriusEntitySymbol }
  | UnresolvedOff { unresolvedSymbol :: AsteriusEntitySymbol
                  , offset :: BinaryenIndex }
  | UnresolvedGetLocal { unresolvedLocalReg :: UnresolvedLocalReg
                       , valueType :: ValueType }
  | UnresolvedSetLocal { unresolvedLocalReg :: UnresolvedLocalReg
                       , value :: AsteriusExpression }
  | UnresolvedTeeLocal { unresolvedLocalReg :: UnresolvedLocalReg
                       , value :: AsteriusExpression }
  deriving (Show, Generic, Data)

instance Serialize UnresolvedExpression

type AsteriusExpression = Expression UnresolvedExpression

data Function = Function
  { functionTypeName :: SBS.ShortByteString
  , varTypes :: V.Vector ValueType
  , body :: Expression Void
  }

data FunctionImport = FunctionImport
  { internalName, externalModuleName, externalBaseName, functionTypeName :: SBS.ShortByteString
  }

data TableImport = TableImport
  { internalName, externalModuleName, externalBaseName :: SBS.ShortByteString
  }

data GlobalImport = GlobalImport
  { internalName, externalModuleName, externalBaseName :: SBS.ShortByteString
  , globalType :: ValueType
  }

data FunctionExport = FunctionExport
  { internalName, externalName :: SBS.ShortByteString
  }

data TableExport = TableExport
  { internalName, externalName :: SBS.ShortByteString
  }

data GlobalExport = GlobalExport
  { internalName, externalName :: SBS.ShortByteString
  }

data Global = Global
  { valueType :: ValueType
  , mutable :: Bool
  , initValue :: Expression Void
  }

newtype FunctionTable = FunctionTable
  { functionNames :: V.Vector SBS.ShortByteString
  }

data DataSegment = DataSegment
  { content :: SBS.ShortByteString
  , offset :: Expression Void
  }

data Memory = Memory
  { initialPages, maximumPages :: BinaryenIndex
  , exportName :: SBS.ShortByteString
  , dataSegments :: V.Vector DataSegment
  }

data Module = Module
  { functionTypeMap :: HM.HashMap SBS.ShortByteString FunctionType
  , functionMap' :: HM.HashMap SBS.ShortByteString Function
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
  }

emptyModule :: Module
emptyModule =
  Module
    { functionTypeMap = []
    , functionMap' = []
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

data RelooperAddBlock a
  = AddBlock { code :: Expression a }
  | AddBlockWithSwitch { code, condition :: Expression a }
  deriving (Show, Generic, Data)

instance Serialize a => Serialize (RelooperAddBlock a)

data RelooperAddBranch a
  = AddBranch { to :: SBS.ShortByteString
              , condition, code :: Expression a }
  | AddBranchForSwitch { to :: SBS.ShortByteString
                       , indexes :: V.Vector BinaryenIndex
                       , code :: Expression a }
  deriving (Show, Generic, Data)

instance Serialize a => Serialize (RelooperAddBranch a)

data RelooperBlock a = RelooperBlock
  { addBlock :: RelooperAddBlock a
  , addBranches :: V.Vector (RelooperAddBranch a)
  } deriving (Show, Generic, Data)

instance Serialize a => Serialize (RelooperBlock a)

data RelooperRun a = RelooperRun
  { entry :: SBS.ShortByteString
  , blockMap :: HM.HashMap SBS.ShortByteString (RelooperBlock a)
  , labelHelper :: BinaryenIndex
  } deriving (Show, Generic, Data)

instance Serialize a => Serialize (RelooperRun a)

instance Serialize SBS.ShortByteString where
  put sbs = put (SBS.length sbs) *> putShortByteString sbs
  {-# INLINEABLE put #-}
  get = get >>= getShortByteString
  {-# INLINEABLE get #-}

instance (Eq k, Hashable k, Serialize k, Serialize v) =>
         Serialize (HM.HashMap k v) where
  put = put . HM.toList
  {-# INLINEABLE put #-}
  get = HM.fromList <$> get
  {-# INLINEABLE get #-}

instance (Eq k, Hashable k, Serialize k) => Serialize (HS.HashSet k) where
  put = put . HS.toList
  {-# INLINEABLE put #-}
  get = HS.fromList <$> get
  {-# INLINEABLE get #-}

instance Serialize a => Serialize (V.Vector a) where
  put v = put (V.length v) *> V.mapM_ put v
  {-# INLINEABLE put #-}
  get = do
    len <- get
    V.replicateM len get
  {-# INLINEABLE get #-}

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt salt = hashWithSalt salt . V.toList
  {-# INLINEABLE hashWithSalt #-}
