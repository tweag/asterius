{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Marshal
  ( MarshalError(..)
  , marshalModule
  , serializeModule
  ) where

import Asterius.Internals
import Asterius.Types
import Bindings.Binaryen.Raw
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BS
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Traversable
import qualified Data.Vector as V
import Foreign
import Foreign.C
import GHC.Exts
import Prelude hiding (IO)

newtype MarshalError =
  UnsupportedExpression Expression
  deriving (Show)

instance Exception MarshalError

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
    ExtendS8Int32 -> c_BinaryenExtendS8Int32
    ExtendS16Int32 -> c_BinaryenExtendS16Int32
    ExtendS8Int64 -> c_BinaryenExtendS8Int64
    ExtendS16Int64 -> c_BinaryenExtendS16Int64
    ExtendS32Int64 -> c_BinaryenExtendS32Int64
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
     Pool
  -> BinaryenModuleRef
  -> SBS.ShortByteString
  -> FunctionType
  -> IO BinaryenFunctionTypeRef
marshalFunctionType pool m k FunctionType {..} = do
  (pts, ptl) <- marshalV pool $ V.map marshalValueType paramTypes
  np <- marshalSBS pool k
  c_BinaryenAddFunctionType m np (marshalValueType returnType) pts ptl

marshalExpression ::
     Pool -> BinaryenModuleRef -> Expression -> IO BinaryenExpressionRef
marshalExpression pool m e =
  case e of
    Block {..} -> do
      bs <- V.forM bodys $ marshalExpression pool m
      (bsp, bl) <- marshalV pool bs
      np <- marshalSBS pool name
      c_BinaryenBlock m np bsp bl (marshalValueType valueType)
    If {..} -> do
      c <- marshalExpression pool m condition
      t <- marshalExpression pool m ifTrue
      f <- marshalExpression pool m ifFalse
      c_BinaryenIf m c t f
    Loop {..} -> do
      b <- marshalExpression pool m body
      np <- marshalSBS pool name
      c_BinaryenLoop m np b
    Break {..} -> do
      c <- marshalExpression pool m condition
      v <- marshalExpression pool m value
      np <- marshalSBS pool name
      c_BinaryenBreak m np c v
    Switch {..} -> do
      c <- marshalExpression pool m condition
      v <- marshalExpression pool m value
      ns <- V.forM names $ marshalSBS pool
      (nsp, nl) <- marshalV pool ns
      dn <- marshalSBS pool defaultName
      c_BinaryenSwitch m nsp nl dn c v
    Call {..} -> do
      os <- V.forM operands $ marshalExpression pool m
      (ops, osl) <- marshalV pool os
      tp <- marshalSBS pool (entityName target)
      c_BinaryenCall m tp ops osl (marshalValueType valueType)
    CallImport {..} -> do
      os <- V.forM operands $ marshalExpression pool m
      (ops, osl) <- marshalV pool os
      tp <- marshalSBS pool target'
      c_BinaryenCallImport m tp ops osl (marshalValueType valueType)
    CallIndirect {..} -> do
      t <- marshalExpression pool m indirectTarget
      os <- V.forM operands $ marshalExpression pool m
      (ops, osl) <- marshalV pool os
      tp <- marshalSBS pool typeName
      c_BinaryenCallIndirect m t ops osl tp
    GetLocal {..} -> c_BinaryenGetLocal m index $ marshalValueType valueType
    SetLocal {..} -> do
      v <- marshalExpression pool m value
      c_BinaryenSetLocal m index v
    TeeLocal {..} -> do
      v <- marshalExpression pool m value
      c_BinaryenTeeLocal m index v
    GetGlobal {..} -> do
      np <- marshalSBS pool name
      c_BinaryenGetGlobal m np (marshalValueType valueType)
    SetGlobal {..} -> do
      v <- marshalExpression pool m value
      np <- marshalSBS pool name
      c_BinaryenSetGlobal m np v
    Load {..} -> do
      p <- marshalExpression pool m ptr
      c_BinaryenLoad
        m
        bytes
        (marshalBool signed)
        offset
        align
        (marshalValueType valueType)
        p
    Store {..} -> do
      p <- marshalExpression pool m ptr
      v <- marshalExpression pool m value
      c_BinaryenStore m bytes offset align p v (marshalValueType valueType)
    ConstI32 x -> c_BinaryenConstInt32 m x
    ConstI64 x -> c_BinaryenConstInt64 m x
    ConstF32 x -> c_BinaryenConstFloat32 m x
    ConstF64 x -> c_BinaryenConstFloat64 m x
    ConstF32Bits x -> c_BinaryenConstFloat32Bits m x
    ConstF64Bits x -> c_BinaryenConstFloat64Bits m x
    Unary {..} -> do
      x <- marshalExpression pool m operand0
      c_BinaryenUnary m (marshalUnaryOp unaryOp) x
    Binary {..} -> do
      x <- marshalExpression pool m operand0
      y <- marshalExpression pool m operand1
      c_BinaryenBinary m (marshalBinaryOp binaryOp) x y
    Select {..} -> do
      c <- marshalExpression pool m condition
      t <- marshalExpression pool m ifTrue
      f <- marshalExpression pool m ifFalse
      c_BinaryenSelect m c t f
    Drop {..} -> do
      v <- marshalExpression pool m value
      c_BinaryenDrop m v
    Return {..} -> do
      v <- marshalExpression pool m value
      c_BinaryenReturn m v
    Host {..} -> do
      xs <- V.forM operands $ marshalExpression pool m
      (es, en) <- marshalV pool xs
      np <- marshalSBS pool name
      c_BinaryenHost m (marshalHostOp hostOp) np es en
    Nop -> c_BinaryenNop m
    Unreachable -> c_BinaryenUnreachable m
    AtomicLoad {..} -> do
      p <- marshalExpression pool m ptr
      c_BinaryenAtomicLoad m bytes offset (marshalValueType valueType) p
    AtomicStore {..} -> do
      p <- marshalExpression pool m ptr
      v <- marshalExpression pool m value
      c_BinaryenAtomicStore m bytes offset p v (marshalValueType valueType)
    AtomicRMW {..} -> do
      p <- marshalExpression pool m ptr
      v <- marshalExpression pool m value
      c_BinaryenAtomicRMW
        m
        (marshalAtomicRMWOp atomicRMWOp)
        bytes
        offset
        p
        v
        (marshalValueType valueType)
    AtomicCmpxchg {..} -> do
      p <- marshalExpression pool m ptr
      o <- marshalExpression pool m expected
      n <- marshalExpression pool m replacement
      c_BinaryenAtomicCmpxchg m bytes offset p o n (marshalValueType valueType)
    CFG {..} -> relooperRun pool m graph
    Null -> pure nullPtr
    _ -> throwIO $ UnsupportedExpression e

marshalFunction ::
     Pool
  -> BinaryenModuleRef
  -> SBS.ShortByteString
  -> BinaryenFunctionTypeRef
  -> Function
  -> IO BinaryenFunctionRef
marshalFunction pool m k ft Function {..} = do
  b <- marshalExpression pool m body
  (vtp, vtl) <- marshalV pool $ V.map marshalValueType varTypes
  np <- marshalSBS pool k
  c_BinaryenAddFunction m np ft vtp vtl b

marshalFunctionImport ::
     Pool
  -> BinaryenModuleRef
  -> BinaryenFunctionTypeRef
  -> FunctionImport
  -> IO BinaryenImportRef
marshalFunctionImport pool m ft FunctionImport {..} = do
  inp <- marshalSBS pool internalName
  emp <- marshalSBS pool externalModuleName
  ebp <- marshalSBS pool externalBaseName
  c_BinaryenAddFunctionImport m inp emp ebp ft

marshalTableImport ::
     Pool -> BinaryenModuleRef -> TableImport -> IO BinaryenImportRef
marshalTableImport pool m TableImport {..} = do
  inp <- marshalSBS pool internalName
  emp <- marshalSBS pool externalModuleName
  ebp <- marshalSBS pool externalBaseName
  c_BinaryenAddTableImport m inp emp ebp

marshalGlobalImport ::
     Pool -> BinaryenModuleRef -> GlobalImport -> IO BinaryenImportRef
marshalGlobalImport pool m GlobalImport {..} = do
  inp <- marshalSBS pool internalName
  emp <- marshalSBS pool externalModuleName
  ebp <- marshalSBS pool externalBaseName
  c_BinaryenAddGlobalImport m inp emp ebp (marshalValueType globalType)

marshalFunctionExport ::
     Pool -> BinaryenModuleRef -> FunctionExport -> IO BinaryenExportRef
marshalFunctionExport pool m FunctionExport {..} = do
  inp <- marshalSBS pool internalName
  enp <- marshalSBS pool externalName
  c_BinaryenAddFunctionExport m inp enp

marshalTableExport ::
     Pool -> BinaryenModuleRef -> TableExport -> IO BinaryenExportRef
marshalTableExport pool m TableExport {..} = do
  inp <- marshalSBS pool internalName
  enp <- marshalSBS pool externalName
  c_BinaryenAddTableExport m inp enp

marshalGlobalExport ::
     Pool -> BinaryenModuleRef -> GlobalExport -> IO BinaryenExportRef
marshalGlobalExport pool m GlobalExport {..} = do
  inp <- marshalSBS pool internalName
  enp <- marshalSBS pool externalName
  c_BinaryenAddGlobalExport m inp enp

marshalGlobal ::
     Pool
  -> BinaryenModuleRef
  -> SBS.ShortByteString
  -> Global
  -> IO BinaryenGlobalRef
marshalGlobal pool m k Global {..} = do
  i <- marshalExpression pool m initValue
  kp <- marshalSBS pool k
  c_BinaryenAddGlobal m kp (marshalValueType valueType) (marshalBool mutable) i

marshalFunctionTable ::
     Pool
  -> BinaryenModuleRef
  -> HM.HashMap SBS.ShortByteString BinaryenFunctionRef
  -> FunctionTable
  -> IO ()
marshalFunctionTable pool m fps FunctionTable {..} = do
  (fnp, fnl) <- marshalV pool $ V.map (fps !) functionNames
  c_BinaryenSetFunctionTable m fnp fnl

marshalMemory :: Pool -> BinaryenModuleRef -> Memory -> IO ()
marshalMemory pool m Memory {..} = do
  (cps, os) <-
    fmap V.unzip $
    V.forM dataSegments $ \DataSegment {..} -> do
      o <- marshalExpression pool m offset
      cp <- marshalSBS pool content
      pure (cp, o)
  (cp, _ :: Int) <- marshalV pool cps
  (ofs, _ :: Int) <- marshalV pool os
  (sps, _ :: Int) <-
    marshalV pool $
    V.map (\DataSegment {..} -> fromIntegral $ SBS.length content) dataSegments
  enp <- marshalSBS pool exportName
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
     BinaryenModuleRef
  -> HM.HashMap SBS.ShortByteString BinaryenFunctionRef
  -> SBS.ShortByteString
  -> IO ()
marshalStartFunctionName m fps n = c_BinaryenSetStart m (fps ! n)

marshalModule :: Pool -> Module -> IO BinaryenModuleRef
marshalModule pool Module {..} = do
  m <- c_BinaryenModuleCreate
  ftps <-
    fmap fromList $
    for (HM.toList functionTypeMap) $ \(k, ft) -> do
      ftp <- marshalFunctionType pool m k ft
      pure (k, ftp)
  fps <-
    fmap fromList $
    for (HM.toList functionMap') $ \(k, f@Function {..}) -> do
      fp <- marshalFunction pool m k (ftps ! functionTypeName) f
      pure (k, fp)
  V.forM_ functionImports $ \fi@FunctionImport {..} ->
    marshalFunctionImport pool m (ftps ! functionTypeName) fi
  V.forM_ tableImports $ marshalTableImport pool m
  V.forM_ globalImports $ marshalGlobalImport pool m
  V.forM_ functionExports $ marshalFunctionExport pool m
  V.forM_ tableExports $ marshalTableExport pool m
  V.forM_ globalExports $ marshalGlobalExport pool m
  for_ (HM.toList globalMap) $ uncurry (marshalGlobal pool m)
  case functionTable of
    Just ft -> marshalFunctionTable pool m fps ft
    _ -> pure ()
  case memory of
    Just mem -> marshalMemory pool m mem
    _ -> pure ()
  case startFunctionName of
    Just k -> marshalStartFunctionName m fps k
    _ -> pure ()
  pure m

relooperAddBlock ::
     Pool
  -> BinaryenModuleRef
  -> RelooperRef
  -> RelooperAddBlock
  -> IO RelooperBlockRef
relooperAddBlock pool m r ab =
  case ab of
    AddBlock {..} -> do
      c <- marshalExpression pool m code
      c_RelooperAddBlock r c
    AddBlockWithSwitch {..} -> do
      _code <- marshalExpression pool m code
      _cond <- marshalExpression pool m condition
      c_RelooperAddBlockWithSwitch r _code _cond

relooperAddBranch ::
     Pool
  -> BinaryenModuleRef
  -> HM.HashMap SBS.ShortByteString RelooperBlockRef
  -> SBS.ShortByteString
  -> RelooperAddBranch
  -> IO ()
relooperAddBranch pool m bm k ab =
  case ab of
    AddBranch {..} -> do
      _cond <- marshalExpression pool m condition
      _code <- marshalExpression pool m code
      c_RelooperAddBranch (bm ! k) (bm ! to) _cond _code
    AddBranchForSwitch {..} -> do
      c <- marshalExpression pool m code
      (idp, idn) <- marshalV pool indexes
      c_RelooperAddBranchForSwitch (bm ! k) (bm ! to) idp idn c

relooperRun ::
     Pool -> BinaryenModuleRef -> RelooperRun -> IO BinaryenExpressionRef
relooperRun pool m RelooperRun {..} = do
  r <- c_RelooperCreate
  bpm <-
    fmap fromList $
    for (HM.toList blockMap) $ \(k, RelooperBlock {..}) -> do
      bp <- relooperAddBlock pool m r addBlock
      pure (k, bp)
  for_ (HM.toList blockMap) $ \(k, RelooperBlock {..}) ->
    V.forM_ addBranches $ relooperAddBranch pool m bpm k
  c_RelooperRenderAndDispose r (bpm ! entry) labelHelper m

serializeModule :: BinaryenModuleRef -> IO BS.ByteString
serializeModule m =
  alloca $ \(buf_p :: Ptr (Ptr ())) ->
    alloca $ \(len_p :: Ptr CSize) ->
      alloca $ \(src_map_p :: Ptr (Ptr CChar)) -> do
        c_BinaryenModuleAllocateAndWriteMut m nullPtr buf_p len_p src_map_p
        buf <- peek buf_p
        len <- peek len_p
        BS.unsafePackMallocCStringLen (castPtr buf, fromIntegral len)
