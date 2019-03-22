{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Marshal
  ( MarshalError(..)
  , marshalModule
  , serializeModule
  ) where

import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import Asterius.TypesConv
import Bindings.Binaryen.Raw
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BS
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Traversable
import Foreign hiding (void)
import Foreign.C
import GHC.Exts
import Language.Haskell.GHC.Toolkit.Constants
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
    I32 -> c_BinaryenTypeInt32
    I64 -> c_BinaryenTypeInt64
    F32 -> c_BinaryenTypeFloat32
    F64 -> c_BinaryenTypeFloat64

marshalReturnTypes :: [ValueType] -> IO BinaryenType
marshalReturnTypes vts =
  case vts of
    [] -> pure c_BinaryenTypeNone
    [vt] -> pure $ marshalValueType vt
    _ ->
      fail $
      "binaryen doesn't support multi-value yet: failed to marshal " <> show vts

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
    CurrentMemory -> c_BinaryenCurrentMemory
    GrowMemory -> c_BinaryenGrowMemory

marshalFunctionType ::
     Pool
  -> BinaryenModuleRef
  -> SBS.ShortByteString
  -> FunctionType
  -> IO BinaryenFunctionTypeRef
marshalFunctionType pool m k FunctionType {..} = do
  (pts, ptl) <- marshalV pool $ map marshalValueType paramTypes
  rts <- marshalReturnTypes returnTypes
  np <- marshalSBS pool k
  c_BinaryenAddFunctionType m np rts pts ptl

marshalExpression ::
     Pool
  -> M.Map AsteriusEntitySymbol Int64
  -> BinaryenModuleRef
  -> Expression
  -> IO BinaryenExpressionRef
marshalExpression pool sym_map m e =
  case e of
    Block {..} -> do
      bs <- forM bodys $ marshalExpression pool sym_map m
      (bsp, bl) <- marshalV pool bs
      np <- marshalSBS pool name
      rts <- marshalReturnTypes blockReturnTypes
      c_BinaryenBlock m np bsp bl rts
    If {..} -> do
      c <- marshalExpression pool sym_map m condition
      t <- marshalExpression pool sym_map m ifTrue
      f <- marshalMaybeExpression pool sym_map m ifFalse
      c_BinaryenIf m c t f
    Loop {..} -> do
      b <- marshalExpression pool sym_map m body
      np <- marshalSBS pool name
      c_BinaryenLoop m np b
    Break {..} -> do
      c <- marshalMaybeExpression pool sym_map m breakCondition
      np <- marshalSBS pool name
      c_BinaryenBreak m np c nullPtr
    Switch {..} -> do
      c <- marshalExpression pool sym_map m condition
      ns <- forM names $ marshalSBS pool
      (nsp, nl) <- marshalV pool ns
      dn <- marshalSBS pool defaultName
      c_BinaryenSwitch m nsp nl dn c nullPtr
    Call {..}
      | M.member target sym_map -> do
        os <- forM operands $ marshalExpression pool sym_map m
        (ops, osl) <- marshalV pool os
        tp <- marshalSBS pool (entityName target)
        rts <- marshalReturnTypes callReturnTypes
        c_BinaryenCall m tp ops osl rts
      | otherwise -> c_BinaryenUnreachable m
    CallImport {..} -> do
      os <- forM operands $ marshalExpression pool sym_map m
      (ops, osl) <- marshalV pool os
      tp <- marshalSBS pool target'
      rts <- marshalReturnTypes callImportReturnTypes
      c_BinaryenCall m tp ops osl rts
    CallIndirect {..} -> do
      t <- marshalExpression pool sym_map m indirectTarget
      os <- forM operands $ marshalExpression pool sym_map m
      (ops, osl) <- marshalV pool os
      tp <- marshalSBS pool $ showSBS functionType
      c_BinaryenCallIndirect m t ops osl tp
    GetLocal {..} -> c_BinaryenGetLocal m index $ marshalValueType valueType
    SetLocal {..} -> do
      v <- marshalExpression pool sym_map m value
      c_BinaryenSetLocal m index v
    Load {..} -> do
      p <- marshalExpression pool sym_map m ptr
      c_BinaryenLoad
        m
        bytes
        (marshalBool signed)
        offset
        1
        (marshalValueType valueType)
        p
    Store {..} -> do
      p <- marshalExpression pool sym_map m ptr
      v <- marshalExpression pool sym_map m value
      c_BinaryenStore m bytes offset 1 p v (marshalValueType valueType)
    ConstI32 x -> c_BinaryenConstInt32 m x
    ConstI64 x -> c_BinaryenConstInt64 m x
    ConstF32 x -> c_BinaryenConstFloat32 m x
    ConstF64 x -> c_BinaryenConstFloat64 m x
    Unary {..} -> do
      x <- marshalExpression pool sym_map m operand0
      c_BinaryenUnary m (marshalUnaryOp unaryOp) x
    Binary {..} -> do
      x <- marshalExpression pool sym_map m operand0
      y <- marshalExpression pool sym_map m operand1
      c_BinaryenBinary m (marshalBinaryOp binaryOp) x y
    ReturnCall {..} -> do
      s <-
        marshalExpression
          pool
          sym_map
          m
          Store
            { bytes = 8
            , offset = 0
            , ptr =
                ConstI32 $
                fromIntegral $ (sym_map ! "__asterius_pc") .&. 0xFFFFFFFF
            , value = ConstI64 $ sym_map !? returnCallTarget64
            , valueType = I64
            }
      r <- c_BinaryenReturn m nullPtr
      arr <- pooledNewArray pool [s, r]
      c_BinaryenBlock m nullPtr arr 2 c_BinaryenTypeNone
    ReturnCallIndirect {..} -> do
      s <-
        marshalExpression
          pool
          sym_map
          m
          Store
            { bytes = 8
            , offset = 0
            , ptr =
                ConstI32 $
                fromIntegral $ (sym_map ! "__asterius_pc") .&. 0xFFFFFFFF
            , value = returnCallIndirectTarget64
            , valueType = I64
            }
      r <- c_BinaryenReturn m nullPtr
      arr <- pooledNewArray pool [s, r]
      c_BinaryenBlock m nullPtr arr 2 c_BinaryenTypeNone
    Host {..} -> do
      xs <- forM operands $ marshalExpression pool sym_map m
      (es, en) <- marshalV pool xs
      c_BinaryenHost m (marshalHostOp hostOp) nullPtr es en
    Nop -> c_BinaryenNop m
    Unreachable -> c_BinaryenUnreachable m
    CFG {..} -> relooperRun pool sym_map m graph
    Symbol {..} ->
      c_BinaryenConstInt64 m $
      case M.lookup unresolvedSymbol sym_map of
        Just x -> x + fromIntegral symbolOffset
        _ -> invalidAddress
    _ -> throwIO $ UnsupportedExpression e

marshalMaybeExpression ::
     Pool
  -> M.Map AsteriusEntitySymbol Int64
  -> BinaryenModuleRef
  -> Maybe Expression
  -> IO BinaryenExpressionRef
marshalMaybeExpression pool sym_map m x =
  case x of
    Just e -> marshalExpression pool sym_map m e
    _ -> pure nullPtr

marshalFunction ::
     Pool
  -> M.Map AsteriusEntitySymbol Int64
  -> BinaryenModuleRef
  -> SBS.ShortByteString
  -> BinaryenFunctionTypeRef
  -> Function
  -> IO BinaryenFunctionRef
marshalFunction pool sym_map m k ft Function {..} = do
  b <- marshalExpression pool sym_map m body
  (vtp, vtl) <- marshalV pool $ map marshalValueType varTypes
  np <- marshalSBS pool k
  c_BinaryenAddFunction m np ft vtp vtl b

marshalFunctionImport ::
     Pool
  -> BinaryenModuleRef
  -> BinaryenFunctionTypeRef
  -> FunctionImport
  -> IO ()
marshalFunctionImport pool m ft FunctionImport {..} = do
  inp <- marshalSBS pool internalName
  emp <- marshalSBS pool externalModuleName
  ebp <- marshalSBS pool externalBaseName
  c_BinaryenAddFunctionImport m inp emp ebp ft

marshalFunctionExport ::
     Pool -> BinaryenModuleRef -> FunctionExport -> IO BinaryenExportRef
marshalFunctionExport pool m FunctionExport {..} = do
  inp <- marshalSBS pool internalName
  enp <- marshalSBS pool externalName
  c_BinaryenAddFunctionExport m inp enp

marshalFunctionTable ::
     Pool -> BinaryenModuleRef -> Int -> FunctionTable -> IO ()
marshalFunctionTable pool m tbl_slots FunctionTable {..} = do
  func_name_ptrs <- for tableFunctionNames $ marshalSBS pool
  (fnp, fnl) <- marshalV pool func_name_ptrs
  c_BinaryenSetFunctionTableWithOffset
    m
    (fromIntegral tbl_slots)
    (-1)
    tableOffset
    fnp
    fnl

marshalMemorySegments ::
     Pool
  -> M.Map AsteriusEntitySymbol Int64
  -> BinaryenModuleRef
  -> Int
  -> [DataSegment]
  -> IO ()
marshalMemorySegments pool sym_map m mbs segs = do
  (seg_bufs, _ :: Int) <- marshalV pool =<< for segs (marshalSBS pool . content)
  (seg_offsets, _ :: Int) <-
    marshalV pool =<<
    for
      segs
      (\DataSegment {..} -> marshalExpression pool sym_map m $ ConstI32 offset)
  (seg_sizes, _ :: Int) <-
    marshalV pool $ map (fromIntegral . SBS.length . content) segs
  c_BinaryenSetMemory
    m
    (fromIntegral $ mbs * (mblock_size `quot` 65536))
    (-1)
    nullPtr
    seg_bufs
    seg_offsets
    seg_sizes
    (fromIntegral $ length segs)
    0

marshalTableImport :: Pool -> BinaryenModuleRef -> TableImport -> IO ()
marshalTableImport pool m TableImport {..} = do
  inp <- marshalSBS pool "0"
  emp <- marshalSBS pool externalModuleName
  ebp <- marshalSBS pool externalBaseName
  c_BinaryenAddTableImport m inp emp ebp

marshalMemoryImport :: Pool -> BinaryenModuleRef -> MemoryImport -> IO ()
marshalMemoryImport pool m MemoryImport {..} = do
  inp <- marshalSBS pool "0"
  emp <- marshalSBS pool externalModuleName
  ebp <- marshalSBS pool externalBaseName
  c_BinaryenAddMemoryImport m inp emp ebp 0

marshalTableExport ::
     Pool -> BinaryenModuleRef -> TableExport -> IO BinaryenExportRef
marshalTableExport pool m TableExport {..} = do
  inp <- marshalSBS pool "0"
  enp <- marshalSBS pool externalName
  c_BinaryenAddTableExport m inp enp

marshalMemoryExport ::
     Pool -> BinaryenModuleRef -> MemoryExport -> IO BinaryenExportRef
marshalMemoryExport pool m MemoryExport {..} = do
  inp <- marshalSBS pool "0"
  enp <- marshalSBS pool externalName
  c_BinaryenAddMemoryExport m inp enp

marshalModule ::
     Pool -> M.Map AsteriusEntitySymbol Int64 -> Module -> IO BinaryenModuleRef
marshalModule pool sym_map hs_mod@Module {..} = do
  let fts = generateWasmFunctionTypeSet hs_mod
  m <- c_BinaryenModuleCreate
  ftps <-
    fmap M.fromList $
    for (Set.toList fts) $ \ft -> do
      ftp <- marshalFunctionType pool m (showSBS ft) ft
      pure (ft, ftp)
  for_ (M.toList functionMap') $ \(k, f@Function {..}) ->
    marshalFunction pool sym_map m k (ftps ! functionType) f
  forM_ functionImports $ \fi@FunctionImport {..} ->
    marshalFunctionImport pool m (ftps ! functionType) fi
  forM_ functionExports $ marshalFunctionExport pool m
  marshalFunctionTable pool m tableSlots functionTable
  marshalTableImport pool m tableImport
  void $ marshalTableExport pool m tableExport
  marshalMemorySegments pool sym_map m memoryMBlocks memorySegments
  marshalMemoryImport pool m memoryImport
  void $ marshalMemoryExport pool m memoryExport
  pure m

relooperAddBlock ::
     Pool
  -> M.Map AsteriusEntitySymbol Int64
  -> BinaryenModuleRef
  -> RelooperRef
  -> RelooperAddBlock
  -> IO RelooperBlockRef
relooperAddBlock pool sym_map m r ab =
  case ab of
    AddBlock {..} -> do
      c <- marshalExpression pool sym_map m code
      c_RelooperAddBlock r c
    AddBlockWithSwitch {..} -> do
      _code <- marshalExpression pool sym_map m code
      _cond <- marshalExpression pool sym_map m condition
      c_RelooperAddBlockWithSwitch r _code _cond

relooperAddBranch ::
     Pool
  -> M.Map AsteriusEntitySymbol Int64
  -> BinaryenModuleRef
  -> M.Map SBS.ShortByteString RelooperBlockRef
  -> SBS.ShortByteString
  -> RelooperAddBranch
  -> IO ()
relooperAddBranch pool sym_map m bm k ab =
  case ab of
    AddBranch {..} -> do
      _cond <- marshalMaybeExpression pool sym_map m addBranchCondition
      c_RelooperAddBranch (bm ! k) (bm ! to) _cond nullPtr
    AddBranchForSwitch {..} -> do
      (idp, idn) <- marshalV pool indexes
      c_RelooperAddBranchForSwitch (bm ! k) (bm ! to) idp idn nullPtr

relooperRun ::
     Pool
  -> M.Map AsteriusEntitySymbol Int64
  -> BinaryenModuleRef
  -> RelooperRun
  -> IO BinaryenExpressionRef
relooperRun pool sym_map m RelooperRun {..} = do
  r <- c_RelooperCreate m
  bpm <-
    fmap fromList $
    for (M.toList blockMap) $ \(k, RelooperBlock {..}) -> do
      bp <- relooperAddBlock pool sym_map m r addBlock
      pure (k, bp)
  for_ (M.toList blockMap) $ \(k, RelooperBlock {..}) ->
    forM_ addBranches $ relooperAddBranch pool sym_map m bpm k
  c_RelooperRenderAndDispose r (bpm ! entry) labelHelper

serializeModule :: BinaryenModuleRef -> IO BS.ByteString
serializeModule m =
  alloca $ \(buf_p :: Ptr (Ptr ())) ->
    alloca $ \(len_p :: Ptr CSize) ->
      alloca $ \(src_map_p :: Ptr (Ptr CChar)) -> do
        c_BinaryenModuleAllocateAndWriteMut m nullPtr buf_p len_p src_map_p
        buf <- peek buf_p
        len <- peek len_p
        BS.unsafePackMallocCStringLen (castPtr buf, fromIntegral len)
