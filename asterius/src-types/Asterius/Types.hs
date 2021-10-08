{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Asterius.Types
  ( BinaryenIndex,
    AsteriusCodeGenError (..),
    AsteriusStatic (..),
    AsteriusStaticsType (..),
    AsteriusStatics (..),
    AsteriusModule (..),
    AsteriusCachedModule (..),
    toCachedModule,
    EntitySymbol,
    entityName,
    mkEntitySymbol,
    UnresolvedLocalReg (..),
    UnresolvedGlobalReg (..),
    ValueType (..),
    Mutability (..),
    GlobalType (..),
    FunctionType (..),
    UnaryOp (..),
    BinaryOp (..),
    FFIHint (..),
    Expression (..),
    Function (..),
    FunctionImport (..),
    TableImport (..),
    MemoryImport (..),
    FunctionExport (..),
    FunctionTable (..),
    DataSegment (..),
    GlobalExport (..),
    GlobalImport (..),
    Global (..),
    Module (..),
    RelooperAddBlock (..),
    RelooperAddBranch (..),
    RelooperBlock (..),
    unreachableRelooperBlock,
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

import Asterius.Binary.Orphans ()
import Asterius.Binary.TH
import Asterius.Monoid.TH
import Asterius.NFData.TH
import Asterius.Semigroup.TH
import Asterius.Types.EntitySymbol
import Asterius.Types.SymbolMap (SymbolMap)
import qualified Asterius.Types.SymbolMap as SM
import Asterius.Types.SymbolSet (SymbolSet)
import qualified Asterius.Types.SymbolSet as SS
import qualified Binary as GHC
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Data
import Data.Foldable
import qualified Data.Map.Lazy as LM
import Foreign
import qualified Type.Reflection as TR

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
  deriving (Show, Data)

instance Exception AsteriusCodeGenError

data AsteriusStatic
  = SymbolStatic EntitySymbol Int
  | Uninitialized Int
  | Serialized BS.ByteString
  deriving (Show, Data)

data AsteriusStaticsType
  = ConstBytes
  | Bytes
  | InfoTable
  | Closure
  deriving (Eq, Show, Data)

data AsteriusStatics
  = AsteriusStatics
      { staticsType :: AsteriusStaticsType,
        asteriusStatics :: [AsteriusStatic]
      }
  deriving (Show, Data)

data AsteriusModule
  = AsteriusModule
      { staticsMap :: SymbolMap AsteriusStatics,
        functionMap :: SymbolMap Function,
        globalsMap :: SymbolMap Global,
        sptMap :: SymbolMap (Word64, Word64),
        ffiMarshalState :: FFIMarshalState
      }
  deriving (Show, Data)

-- | An 'AsteriusCachedModule' in an 'AsteriusModule' along with  with all of
-- its 'EntitySymbol' dependencies, as they are appear in the modules data
-- segments and function definitions (see function 'toCachedModule').
data AsteriusCachedModule
  = AsteriusCachedModule
      { dependencyMap :: SymbolMap SymbolSet,
        fromCachedModule :: AsteriusModule
      }
  deriving (Show, Data)

instance GHC.Binary AsteriusCachedModule where
  get bh = do
    getObjectMagic bh
    dependencyMap <- GHC.get bh
    fromCachedModule <- GHC.get bh
    pure AsteriusCachedModule {..}

  put_ bh AsteriusCachedModule {..} = do
    putObjectMagic bh
    GHC.put_ bh dependencyMap
    GHC.put_ bh fromCachedModule

objectMagic :: BS.ByteString
objectMagic = "!<asterius>\n"

putObjectMagic :: GHC.BinHandle -> IO ()
putObjectMagic bh = for_ (BS.unpack objectMagic) (GHC.putByte bh)

getObjectMagic :: GHC.BinHandle -> IO ()
getObjectMagic bh = do
  magic <- replicateM (BS.length objectMagic) (GHC.getByte bh)
  when (BS.pack magic /= objectMagic) $
    fail "Not an Asterius object file."

-- | Convert an 'AsteriusModule' to an 'AsteriusCachedModule' by laboriously
-- computing the dependency graph for each 'EntitySymbol'. Historical note: we
-- used to compute the dependency graph during link time but that were quite
-- inefficient (see isssue #568). Instead, we now do the same work at
-- compile-time, thus creating object files containing 'AsteriusCachedModule's
-- instead of 'AsteriusModule's.
toCachedModule :: AsteriusModule -> AsteriusCachedModule
toCachedModule m =
  AsteriusCachedModule
    { fromCachedModule = m,
      dependencyMap = staticsMap m `add` (functionMap m `add` (globalsMap m `add` SM.empty))
    }
  where
    add :: Data a => SymbolMap a -> SymbolMap SymbolSet -> SymbolMap SymbolSet
    add = flip $ SM.foldrWithKey' (\k e -> SM.insert k (collectEntitySymbols e))
    -- Collect all entity symbols from an entity.
    collectEntitySymbols :: Data a => a -> SymbolSet
    collectEntitySymbols t
      | Just TR.HRefl <- TR.eqTypeRep (TR.typeOf t) (TR.typeRep @EntitySymbol) =
        SS.singleton t
      | otherwise =
        gmapQl (<>) SS.empty collectEntitySymbols t

data UnresolvedLocalReg
  = UniqueLocalReg Int ValueType
  | QuotRemI32X
  | QuotRemI32Y
  | QuotRemI64X
  | QuotRemI64Y
  deriving (Eq, Ord, Show, Data)

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
  deriving (Show, Data)

data ValueType
  = I32
  | I64
  | F32
  | F64
  deriving (Eq, Ord, Enum, Show, Data)

data FunctionType
  = FunctionType
      { paramTypes, returnTypes :: [ValueType]
      }
  deriving (Eq, Ord, Show, Data)

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
  deriving (Show, Data)

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
  deriving (Show, Data)

-- | 'Mutability' of variables.
data Mutability
  = Mutable
  | Immutable
  deriving (Eq, Ord, Show, Data)

-- | 'GlobalType's classify global variables which hold a value and can either
-- be mutable or immutable.
data GlobalType
  = GlobalType
      { globalValueType :: ValueType,
        globalMutability :: Mutability
      }
  deriving (Eq, Ord, Show, Data)

data FFIHint
  = NoHint
  | AddrHint
  | SignedHint
  deriving (Show, Data)

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
        callReturnTypes :: [ValueType],
        callHint :: Maybe ([FFIHint], [FFIHint])
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
  | GetGlobal
      { globalSymbol :: EntitySymbol,
        valueType :: ValueType
      }
  | SetGlobal
      { globalSymbol :: EntitySymbol,
        value :: Expression
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
  deriving (Show, Data)

-- | A 'Global' captures a single global variable. Each 'Global' stores a
-- single value of the given global type.  Moreover, each 'Global' is
-- initialized with an initial value, given by a constant initializer
-- expression.
data Global
  = Global
      { globalType :: GlobalType,
        globalInit :: Expression
      }
  deriving (Show, Data)

data Function
  = Function
      { functionType :: FunctionType,
        varTypes :: [ValueType],
        body :: Expression
      }
  deriving (Show, Data)

data FunctionImport
  = FunctionImport
      { internalName, externalModuleName, externalBaseName :: BS.ByteString,
        functionType :: FunctionType
      }
  deriving (Show, Data)

data TableImport
  = TableImport
      { externalModuleName, externalBaseName :: BS.ByteString
      }
  deriving (Show, Data)

data MemoryImport
  = MemoryImport
      { externalModuleName, externalBaseName :: BS.ByteString
      }
  deriving (Show, Data)

data FunctionExport
  = FunctionExport
      { internalName, externalName :: BS.ByteString
      }
  deriving (Show, Data)

data FunctionTable
  = FunctionTable
      { tableFunctionNames :: [BS.ByteString],
        tableOffset :: Expression
      }
  deriving (Show, Data)

data DataSegment
  = DataSegment
      { content :: BS.ByteString,
        offset :: Expression
      }
  deriving (Show, Data)

data GlobalExport
  = GlobalExport
      { internalName, externalName :: BS.ByteString
      }
  deriving (Show, Data)

data GlobalImport
  = GlobalImport
      { internalName, externalModuleName, externalBaseName :: BS.ByteString,
        globalType :: GlobalType
      }
  deriving (Show, Data)

data Module
  = Module
      { functionMap' :: LM.Map BS.ByteString Function,
        functionImports :: [FunctionImport],
        functionExports :: [FunctionExport],
        functionTable :: FunctionTable,
        tableImport :: Maybe TableImport,
        tableSlots :: Int,
        globalImports :: [GlobalImport],
        globalExports :: [GlobalExport],
        globalMap :: SymbolMap Global,
        memorySegments :: [DataSegment],
        memoryImport :: Maybe MemoryImport
      }
  deriving (Show, Data)

data RelooperAddBlock
  = AddBlock
      { code :: Expression
      }
  | AddBlockWithSwitch
      { code, condition :: Expression
      }
  deriving (Show, Data)

data RelooperAddBranch
  = AddBranch
      { to :: BS.ByteString,
        addBranchCondition :: Maybe Expression
      }
  | AddBranchForSwitch
      { to :: BS.ByteString,
        indexes :: [BinaryenIndex]
      }
  deriving (Show, Data)

data RelooperBlock
  = RelooperBlock
      { addBlock :: RelooperAddBlock,
        addBranches :: [RelooperAddBranch]
      }
  deriving (Show, Data)

-- | A 'RelooperBlock' containing a single 'Unreachable' instruction.
unreachableRelooperBlock :: RelooperBlock
unreachableRelooperBlock =
  RelooperBlock -- See Note [unreachableRelooperBlock]
    { addBlock =
        AddBlock
          { code = Unreachable
          },
      addBranches = []
    }

data RelooperRun
  = RelooperRun
      { entry :: BS.ByteString,
        blockMap :: LM.Map BS.ByteString RelooperBlock,
        labelHelper :: BinaryenIndex
      }
  deriving (Show, Data)

data FFIValueTypeRep
  = FFIJSValRep
  | FFILiftedRep
  | FFIUnliftedRep
  | FFIIntRep
  | FFIInt8Rep
  | FFIInt16Rep
  | FFIInt32Rep
  | FFIInt64Rep
  | FFIWordRep
  | FFIWord8Rep
  | FFIWord16Rep
  | FFIWord32Rep
  | FFIWord64Rep
  | FFIAddrRep
  | FFIFloatRep
  | FFIDoubleRep
  deriving (Show, Data)

data FFIValueType
  = FFIValueType
      { ffiValueTypeRep :: FFIValueTypeRep,
        hsTyCon :: BS.ByteString
      }
  deriving (Show, Data)

data FFIFunctionType
  = FFIFunctionType
      { ffiParamTypes, ffiResultTypes :: [FFIValueType],
        ffiInIO :: Bool
      }
  deriving (Show, Data)

data FFISafety
  = FFIUnsafe
  | FFISafe
  | FFIInterruptible
  deriving (Eq, Show, Data)

data FFIImportDecl
  = FFIImportDecl
      { ffiFunctionType :: FFIFunctionType,
        ffiSafety :: FFISafety,
        ffiSourceText :: BS.ByteString
      }
  deriving (Show, Data)

data FFIExportDecl
  = FFIExportDecl
      { ffiFunctionType :: FFIFunctionType,
        ffiExportClosure :: EntitySymbol
      }
  deriving (Show, Data)

data FFIMarshalState
  = FFIMarshalState
      { ffiImportDecls :: SymbolMap FFIImportDecl,
        ffiExportDecls :: SymbolMap FFIExportDecl
      }
  deriving (Show, Data)

-- NFData instances

$(genNFData ''AsteriusCodeGenError)

$(genNFData ''AsteriusStatic)

$(genNFData ''AsteriusStaticsType)

$(genNFData ''AsteriusStatics)

$(genNFData ''AsteriusModule)

$(genNFData ''AsteriusCachedModule)

$(genNFData ''UnresolvedLocalReg)

$(genNFData ''UnresolvedGlobalReg)

$(genNFData ''ValueType)

$(genNFData ''FunctionType)

$(genNFData ''UnaryOp)

$(genNFData ''BinaryOp)

$(genNFData ''Mutability)

$(genNFData ''GlobalType)

$(genNFData ''Global)

$(genNFData ''FFIHint)

$(genNFData ''Expression)

$(genNFData ''Function)

$(genNFData ''FunctionImport)

$(genNFData ''TableImport)

$(genNFData ''MemoryImport)

$(genNFData ''FunctionExport)

$(genNFData ''FunctionTable)

$(genNFData ''DataSegment)

$(genNFData ''GlobalImport)

$(genNFData ''GlobalExport)

$(genNFData ''Module)

$(genNFData ''RelooperAddBlock)

$(genNFData ''RelooperAddBranch)

$(genNFData ''RelooperBlock)

$(genNFData ''RelooperRun)

$(genNFData ''FFIValueTypeRep)

$(genNFData ''FFIValueType)

$(genNFData ''FFIFunctionType)

$(genNFData ''FFISafety)

$(genNFData ''FFIImportDecl)

$(genNFData ''FFIExportDecl)

$(genNFData ''FFIMarshalState)

-- Binary instances

$(genBinary ''AsteriusCodeGenError)

$(genBinary ''AsteriusStatic)

$(genBinary ''AsteriusStaticsType)

$(genBinary ''AsteriusStatics)

$(genBinary ''AsteriusModule)

$(genBinary ''UnresolvedLocalReg)

$(genBinary ''UnresolvedGlobalReg)

$(genBinary ''ValueType)

$(genBinary ''FunctionType)

$(genBinary ''UnaryOp)

$(genBinary ''BinaryOp)

$(genBinary ''Mutability)

$(genBinary ''GlobalType)

$(genBinary ''Global)

$(genBinary ''FFIHint)

$(genBinary ''Expression)

$(genBinary ''Function)

$(genBinary ''FunctionImport)

$(genBinary ''TableImport)

$(genBinary ''MemoryImport)

$(genBinary ''FunctionExport)

$(genBinary ''FunctionTable)

$(genBinary ''DataSegment)

$(genBinary ''GlobalImport)

$(genBinary ''GlobalExport)

$(genBinary ''Module)

$(genBinary ''RelooperAddBlock)

$(genBinary ''RelooperAddBranch)

$(genBinary ''RelooperBlock)

$(genBinary ''RelooperRun)

$(genBinary ''FFIValueTypeRep)

$(genBinary ''FFIValueType)

$(genBinary ''FFIFunctionType)

$(genBinary ''FFISafety)

$(genBinary ''FFIImportDecl)

$(genBinary ''FFIExportDecl)

$(genBinary ''FFIMarshalState)

-- Semigroup instances

$(genSemigroup ''AsteriusModule)

$(genSemigroup ''AsteriusCachedModule)

$(genSemigroup ''FFIMarshalState)

-- Semigroup instances

$(genMonoid ''AsteriusModule)

$(genMonoid ''AsteriusCachedModule)

$(genMonoid ''FFIMarshalState)
