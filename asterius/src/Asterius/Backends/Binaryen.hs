{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

-- |
-- Module      :  Asterius.Backends.Binaryen
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Elaboration of Asterius types into the Binaryen AST (as defined in the
-- [binaryen package](https://github.com/tweag/haskell-binaryen)).
module Asterius.Backends.Binaryen
  ( MarshalError (..),
    marshalModule,
    serializeModule,
    serializeModuleSExpr,
    setColorsEnabled,
  )
where

import Asterius.Internals
import Asterius.Internals.Barf
import Asterius.Internals.MagicNumber
import Asterius.Internals.Marshal
import Asterius.Types
import Asterius.TypesConv
import Bindings.Binaryen.Raw
import Control.Exception
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Traversable
import Foreign hiding
  ( void,
    withPool,
  )
import Foreign.C
import GHC.Exts
import Language.Haskell.GHC.Toolkit.Constants

newtype MarshalError
  = UnsupportedExpression Expression
  deriving (Show)

instance Exception MarshalError

marshalBool :: Bool -> Int8
marshalBool flag = if flag then 1 else 0

marshalValueType :: ValueType -> BinaryenType
marshalValueType t = case t of
  I32 -> c_BinaryenTypeInt32
  I64 -> c_BinaryenTypeInt64
  F32 -> c_BinaryenTypeFloat32
  F64 -> c_BinaryenTypeFloat64

marshalReturnTypes :: [ValueType] -> IO BinaryenType
marshalReturnTypes vts = case vts of
  [] -> pure c_BinaryenTypeNone
  [vt] -> pure $ marshalValueType vt
  _ ->
    fail $
      "binaryen doesn't support multi-value yet: failed to marshal "
        <> show vts

marshalUnaryOp :: UnaryOp -> BinaryenOp
marshalUnaryOp op = case op of
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
marshalBinaryOp op = case op of
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
marshalHostOp op = case op of
  CurrentMemory -> c_BinaryenMemorySize
  GrowMemory -> c_BinaryenMemoryGrow

marshalFunctionType :: FunctionType -> IO (BinaryenType, BinaryenType)
marshalFunctionType FunctionType {..} = flip runContT pure $ do
  (pts, ptl) <- marshalV $ map marshalValueType paramTypes
  (rts, rtl) <- marshalV $ map marshalValueType returnTypes
  lift $ do
    pt <- c_BinaryenTypeCreate pts (fromIntegral ptl)
    rt <- c_BinaryenTypeCreate rts (fromIntegral rtl)
    pure (pt, rt)

-- TODO: there is a similar definition in Asterius.Backends.WasmToolkit. Maybe
-- we should just move shared stuff into Asterius.Types (or another shared file).
type SymbolMap = M.Map EntitySymbol Int64

-- | Environment used during marshaling of Asterius' types to Binaryen.
data MarshalEnv
  = MarshalEnv
      { -- | Whether the tail call extension is on.
        envAreTailCallsOn :: Bool,
        -- | The symbol map for the current module.
        envSymbolMap :: SymbolMap,
        -- | The current module reference.
        envModuleRef :: BinaryenModuleRef
      }

type CodeGen a = ReaderT MarshalEnv IO a

-- | Check whether the tail call extension is on.
areTailCallsOn :: CodeGen Bool
areTailCallsOn = reader envAreTailCallsOn

-- | Retrieve the symbol map from the local environment.
askSymbolMap :: CodeGen SymbolMap
askSymbolMap = reader envSymbolMap

-- | Retrieve the reference to the current module.
askModuleRef :: CodeGen BinaryenModuleRef
askModuleRef = reader envModuleRef

marshalExpression :: Expression -> CodeGen BinaryenExpressionRef
marshalExpression e = case e of
  Block {..} -> do
    env <- ask
    m <- askModuleRef
    lift $ flip runContT pure $ do
      bs <- lift $ flip runReaderT env $ forM bodys marshalExpression
      (bsp, bl) <- marshalV bs
      np <- marshalBS name
      rts <- lift $ marshalReturnTypes blockReturnTypes
      lift $ c_BinaryenBlock m np bsp (fromIntegral bl) rts
  If {..} -> do
    c <- marshalExpression condition
    t <- marshalExpression ifTrue
    f <- marshalMaybeExpression ifFalse
    m <- askModuleRef
    lift $ c_BinaryenIf m c t f
  Loop {..} -> do
    b <- marshalExpression body
    m <- askModuleRef
    lift $ flip runContT pure $ do
      np <- marshalBS name
      lift $ c_BinaryenLoop m np b
  Break {..} -> do
    c <- marshalMaybeExpression breakCondition
    m <- askModuleRef
    lift $ flip runContT pure $ do
      np <- marshalBS name
      lift $ c_BinaryenBreak m np c nullPtr
  Switch {..} -> do
    c <- marshalExpression condition
    m <- askModuleRef
    lift $ flip runContT pure $ do
      ns <- forM names marshalBS
      (nsp, nl) <- marshalV ns
      dn <- marshalBS defaultName
      lift $ c_BinaryenSwitch m nsp (fromIntegral nl) dn c nullPtr
  Call {..} -> do
    sym_map <- askSymbolMap
    if  | M.member target sym_map ->
          do
            os <-
              mapM
                marshalExpression
                ( if target == "barf"
                    then
                      [ case operands of
                          [] -> ConstI64 0
                          x : _ -> x
                      ]
                    else operands
                )
            m <- askModuleRef
            lift $ flip runContT pure $ do
              (ops, osl) <- marshalV os
              tp <- marshalBS (entityName target)
              rts <- lift $ marshalReturnTypes callReturnTypes
              lift $ c_BinaryenCall m tp ops (fromIntegral osl) rts
        | M.member ("__asterius_barf_" <> target) sym_map ->
          marshalExpression $ barf target callReturnTypes
        | otherwise -> do
          m <- askModuleRef
          lift $ c_BinaryenUnreachable m
  CallImport {..} -> do
    os <- forM operands marshalExpression
    m <- askModuleRef
    lift $ flip runContT pure $ do
      (ops, osl) <- marshalV os
      tp <- marshalBS target'
      rts <- lift $ marshalReturnTypes callImportReturnTypes
      lift $ c_BinaryenCall m tp ops (fromIntegral osl) rts
  CallIndirect {..} -> do
    t <- marshalExpression indirectTarget
    os <- forM operands marshalExpression
    m <- askModuleRef
    lift $ flip runContT pure $ do
      (ops, osl) <- marshalV os
      lift $ do
        (pt, rt) <- marshalFunctionType functionType
        c_BinaryenCallIndirect m t ops (fromIntegral osl) pt rt
  GetLocal {..} -> do
    m <- askModuleRef
    lift $ c_BinaryenLocalGet m index $ marshalValueType valueType
  SetLocal {..} -> do
    v <- marshalExpression value
    m <- askModuleRef
    lift $ c_BinaryenLocalSet m index v
  TeeLocal {..} -> do
    v <- marshalExpression value
    m <- askModuleRef
    lift $ c_BinaryenLocalTee m index v $ marshalValueType valueType
  Load {..} -> do
    p <- marshalExpression ptr
    m <- askModuleRef
    lift $
      c_BinaryenLoad
        m
        bytes
        (marshalBool signed)
        offset
        1
        (marshalValueType valueType)
        p
  Store {..} -> do
    p <- marshalExpression ptr
    v <- marshalExpression value
    m <- askModuleRef
    lift $ c_BinaryenStore m bytes offset 1 p v (marshalValueType valueType)
  ConstI32 x -> do
    m <- askModuleRef
    lift $ c_BinaryenConstInt32 m x
  ConstI64 x -> do
    m <- askModuleRef
    lift $ c_BinaryenConstInt64 m x
  ConstF32 x -> do
    m <- askModuleRef
    lift $ c_BinaryenConstFloat32 m x
  ConstF64 x -> do
    m <- askModuleRef
    lift $ c_BinaryenConstFloat64 m x
  Unary {..} -> do
    x <- marshalExpression operand0
    m <- askModuleRef
    lift $ c_BinaryenUnary m (marshalUnaryOp unaryOp) x
  Binary {..} -> do
    x <- marshalExpression operand0
    y <- marshalExpression operand1
    m <- askModuleRef
    lift $ c_BinaryenBinary m (marshalBinaryOp binaryOp) x y
  Drop {..} -> do
    x <- marshalExpression dropValue
    m <- askModuleRef
    lift $ c_BinaryenDrop m x
  ReturnCall {..} -> areTailCallsOn >>= \case
    -- Case 1: Tail calls are on
    True -> do
      m <- askModuleRef
      lift $ flip runContT pure $ do
        dst <- marshalBS (entityName returnCallTarget64)
        lift $ c_BinaryenReturnCall m dst nullPtr 0 c_BinaryenTypeNone
    -- Case 2: Tail calls are off
    False -> do
      sym_map <- askSymbolMap
      case M.lookup returnCallTarget64 sym_map of
        Just t -> do
          s <-
            marshalExpression
              Store
                { bytes = 8,
                  offset = 0,
                  ptr =
                    ConstI32
                      $ fromIntegral
                      $ (sym_map ! "__asterius_pc")
                        .&. 0xFFFFFFFF,
                  value = ConstI64 t,
                  valueType = I64
                }
          m <- askModuleRef
          lift $ flip runContT pure $ do
            r <- lift $ c_BinaryenReturn m nullPtr
            (arr, _) <- marshalV [s, r]
            lift $ c_BinaryenBlock m nullPtr arr 2 c_BinaryenTypeNone
        Nothing -> marshalExpression $ barf returnCallTarget64 []
  ReturnCallIndirect {..} -> areTailCallsOn >>= \case
    -- Case 1: Tail calls are on
    True -> do
      t <-
        marshalExpression
          Unary {unaryOp = WrapInt64, operand0 = returnCallIndirectTarget64}
      m <- askModuleRef
      lift $
        c_BinaryenReturnCallIndirect
          m
          t
          nullPtr
          0
          c_BinaryenTypeNone
          c_BinaryenTypeNone
    -- Case 2: Tail calls are off
    False -> do
      sym_map <- askSymbolMap
      s <-
        marshalExpression
          Store
            { bytes = 8,
              offset = 0,
              ptr =
                ConstI32
                  $ fromIntegral
                  $ (sym_map ! "__asterius_pc")
                    .&. 0xFFFFFFFF,
              value = returnCallIndirectTarget64,
              valueType = I64
            }
      m <- askModuleRef
      lift $ flip runContT pure $ do
        r <- lift $ c_BinaryenReturn m nullPtr
        (arr, _) <- marshalV [s, r]
        lift $ c_BinaryenBlock m nullPtr arr 2 c_BinaryenTypeNone
  Host {..} -> do
    xs <- forM operands marshalExpression
    m <- askModuleRef
    lift $ flip runContT pure $ do
      (es, en) <- marshalV xs
      lift $ c_BinaryenHost m (marshalHostOp hostOp) nullPtr es (fromIntegral en)
  Nop -> do
    m <- askModuleRef
    lift $ c_BinaryenNop m
  Unreachable -> do
    m <- askModuleRef
    lift $ c_BinaryenUnreachable m
  CFG {..} -> relooperRun graph
  Symbol {..} -> do
    sym_map <- askSymbolMap
    m <- askModuleRef
    case M.lookup unresolvedSymbol sym_map of
      Just x -> lift $ c_BinaryenConstInt64 m $ x + fromIntegral symbolOffset
      _
        | M.member ("__asterius_barf_" <> unresolvedSymbol) sym_map ->
          marshalExpression $ barf unresolvedSymbol [I64]
        | otherwise ->
          lift $ c_BinaryenConstInt64 m invalidAddress
  -- Unsupported expressions
  UnresolvedGetLocal {} -> lift $ throwIO $ UnsupportedExpression e
  UnresolvedSetLocal {} -> lift $ throwIO $ UnsupportedExpression e
  Barf {} -> lift $ throwIO $ UnsupportedExpression e

marshalMaybeExpression :: Maybe Expression -> CodeGen BinaryenExpressionRef
marshalMaybeExpression x = case x of
  Just e -> marshalExpression e
  _ -> pure nullPtr

marshalFunction ::
  BS.ByteString ->
  (BinaryenType, BinaryenType) ->
  Function ->
  CodeGen BinaryenFunctionRef
marshalFunction k (pt, rt) Function {..} = do
  env <- ask
  m <- askModuleRef
  lift $ flip runContT pure $ do
    b <- lift $ flip runReaderT env $ marshalExpression body
    (vtp, vtl) <- marshalV $ map marshalValueType varTypes
    np <- marshalBS k
    lift $ c_BinaryenAddFunction m np pt rt vtp (fromIntegral vtl) b

marshalFunctionImport ::
  BinaryenModuleRef ->
  (BinaryenType, BinaryenType) ->
  FunctionImport ->
  IO ()
marshalFunctionImport m (pt, rt) FunctionImport {..} = flip runContT pure $ do
  inp <- marshalBS internalName
  emp <- marshalBS externalModuleName
  ebp <- marshalBS externalBaseName
  lift $ c_BinaryenAddFunctionImport m inp emp ebp pt rt

marshalFunctionExport ::
  BinaryenModuleRef -> FunctionExport -> IO BinaryenExportRef
marshalFunctionExport m FunctionExport {..} = flip runContT pure $ do
  inp <- marshalBS internalName
  enp <- marshalBS externalName
  lift $ c_BinaryenAddFunctionExport m inp enp

marshalFunctionTable :: BinaryenModuleRef -> Int -> FunctionTable -> IO ()
marshalFunctionTable m tbl_slots FunctionTable {..} = flip runContT pure $ do
  func_name_ptrs <- for tableFunctionNames marshalBS
  (fnp, fnl) <- marshalV func_name_ptrs
  lift $ do
    o <- c_BinaryenConstInt32 m (fromIntegral tableOffset)
    c_BinaryenSetFunctionTable
      m
      (fromIntegral tbl_slots)
      (-1)
      fnp
      (fromIntegral fnl)
      o

marshalMemorySegments :: Int -> [DataSegment] -> CodeGen ()
marshalMemorySegments mbs segs = do
  env <- ask
  m <- askModuleRef
  let segs_len = length segs
  lift $ flip runContT pure $ do
    (seg_bufs, _) <- marshalV =<< for segs (marshalBS . content)
    (seg_passives, _) <- marshalV $ replicate segs_len 0
    (seg_offsets, _) <-
      marshalV
        =<< for
          segs
          ( \DataSegment {..} ->
              lift $ flip runReaderT env $ marshalExpression $ ConstI32 offset
          )
    (seg_sizes, _) <-
      marshalV $
        map (fromIntegral . BS.length . content) segs
    lift $
      c_BinaryenSetMemory
        m
        (fromIntegral $ mbs * (mblock_size `quot` 65536))
        (-1)
        nullPtr
        seg_bufs
        seg_passives
        seg_offsets
        seg_sizes
        (fromIntegral segs_len)
        0

marshalTableImport :: BinaryenModuleRef -> TableImport -> IO ()
marshalTableImport m TableImport {..} = flip runContT pure $ do
  inp <- marshalBS "0"
  emp <- marshalBS externalModuleName
  ebp <- marshalBS externalBaseName
  lift $ c_BinaryenAddTableImport m inp emp ebp

marshalMemoryImport :: BinaryenModuleRef -> MemoryImport -> IO ()
marshalMemoryImport m MemoryImport {..} = flip runContT pure $ do
  inp <- marshalBS "0"
  emp <- marshalBS externalModuleName
  ebp <- marshalBS externalBaseName
  lift $ c_BinaryenAddMemoryImport m inp emp ebp 0

marshalTableExport :: BinaryenModuleRef -> TableExport -> IO BinaryenExportRef
marshalTableExport m TableExport {..} = flip runContT pure $ do
  inp <- marshalBS "0"
  enp <- marshalBS externalName
  lift $ c_BinaryenAddTableExport m inp enp

marshalMemoryExport :: BinaryenModuleRef -> MemoryExport -> IO BinaryenExportRef
marshalMemoryExport m MemoryExport {..} = flip runContT pure $ do
  inp <- marshalBS "0"
  enp <- marshalBS externalName
  lift $ c_BinaryenAddMemoryExport m inp enp

marshalModule ::
  Bool -> M.Map EntitySymbol Int64 -> Module -> IO BinaryenModuleRef
marshalModule tail_calls sym_map hs_mod@Module {..} = do
  let fts = generateWasmFunctionTypeSet hs_mod
  m <- c_BinaryenModuleCreate
  c_BinaryenModuleSetFeatures m
    $ foldl1' (.|.)
    $ [c_BinaryenFeatureTailCall | tail_calls]
      <> [c_BinaryenFeatureMVP]
  ftps <- fmap M.fromList $ for (Set.toList fts) $ \ft -> do
    ftp <- marshalFunctionType ft
    pure (ft, ftp)
  let env =
        MarshalEnv
          { envAreTailCallsOn = tail_calls,
            envSymbolMap = sym_map,
            envModuleRef = m
          }
  for_ (M.toList functionMap') $ \(k, f@Function {..}) ->
    flip runReaderT env $ marshalFunction k (ftps ! functionType) f
  forM_ functionImports $ \fi@FunctionImport {..} ->
    marshalFunctionImport m (ftps ! functionType) fi
  forM_ functionExports $ marshalFunctionExport m
  marshalFunctionTable m tableSlots functionTable
  marshalTableImport m tableImport
  void $ marshalTableExport m tableExport
  flip runReaderT env $ marshalMemorySegments memoryMBlocks memorySegments
  marshalMemoryImport m memoryImport
  void $ marshalMemoryExport m memoryExport
  flip runContT pure $ do
    lim_segs <- marshalBS "limit-segments"
    (lim_segs_p, _) <- marshalV [lim_segs]
    lift $ c_BinaryenModuleRunPasses m lim_segs_p 1
  pure m

relooperAddBlock :: RelooperRef -> RelooperAddBlock -> CodeGen RelooperBlockRef
relooperAddBlock r ab = case ab of
  AddBlock {..} -> do
    c <- marshalExpression code
    lift $ c_RelooperAddBlock r c
  AddBlockWithSwitch {..} -> do
    _code <- marshalExpression code
    _cond <- marshalExpression condition
    lift $ c_RelooperAddBlockWithSwitch r _code _cond

relooperAddBranch ::
  M.Map BS.ByteString RelooperBlockRef ->
  BS.ByteString ->
  RelooperAddBranch ->
  CodeGen ()
relooperAddBranch bm k ab = case ab of
  AddBranch {..} -> do
    _cond <- marshalMaybeExpression addBranchCondition
    lift $ c_RelooperAddBranch (bm ! k) (bm ! to) _cond nullPtr
  AddBranchForSwitch {..} -> lift $ flip runContT pure $ do
    (idp, idn) <- marshalV indexes
    lift $
      c_RelooperAddBranchForSwitch
        (bm ! k)
        (bm ! to)
        idp
        (fromIntegral idn)
        nullPtr

relooperRun :: RelooperRun -> CodeGen BinaryenExpressionRef
relooperRun RelooperRun {..} = do
  m <- askModuleRef
  r <- lift $ c_RelooperCreate m
  bpm <- fmap fromList $ for (M.toList blockMap) $ \(k, RelooperBlock {..}) ->
    do
      bp <- relooperAddBlock r addBlock
      pure (k, bp)
  for_ (M.toList blockMap) $ \(k, RelooperBlock {..}) ->
    forM_ addBranches $ relooperAddBranch bpm k
  lift $ c_RelooperRenderAndDispose r (bpm ! entry) labelHelper

serializeModule :: BinaryenModuleRef -> IO BS.ByteString
serializeModule m = alloca $ \(buf_p :: Ptr (Ptr ())) ->
  alloca $ \(len_p :: Ptr CSize) -> alloca $ \(src_map_p :: Ptr (Ptr CChar)) ->
    do
      c_BinaryenModuleAllocateAndWriteMut m nullPtr buf_p len_p src_map_p
      buf <- peek buf_p
      len <- peek len_p
      BS.unsafePackMallocCStringLen (castPtr buf, fromIntegral len)

serializeModuleSExpr :: BinaryenModuleRef -> IO BS.ByteString
serializeModuleSExpr m =
  c_BinaryenModuleAllocateAndWriteText m >>= BS.unsafePackCString

setColorsEnabled :: Bool -> IO ()
setColorsEnabled b = c_BinaryenSetColorsEnabled . toEnum . fromEnum $ b
