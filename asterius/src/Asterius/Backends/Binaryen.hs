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
-- [binaryen package](https://github.com/tweag/binaryen)).
module Asterius.Backends.Binaryen
  ( MarshalError (..),
    marshalModule,
    serializeModule,
    serializeModuleSExpr,
    setColorsEnabled,
  )
where

import Asterius.Internals.Barf
import Asterius.Internals.MagicNumber
import Asterius.Internals.Marshal
import Asterius.Internals.Parallel
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Asterius.TypesConv
import qualified Binaryen
import qualified Binaryen.Expression
import qualified Binaryen.Expression as Binaryen
import qualified Binaryen.Features as Binaryen
import qualified Binaryen.Index as Binaryen
import qualified Binaryen.Module
import qualified Binaryen.Module as Binaryen
import qualified Binaryen.Op as Binaryen
import qualified Binaryen.Relooper
import qualified Binaryen.Relooper as Binaryen
import qualified Binaryen.Type
import qualified Binaryen.Type as Binaryen
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

marshalValueType :: ValueType -> Binaryen.Type
marshalValueType t = case t of
  I32 -> Binaryen.int32
  I64 -> Binaryen.int64
  F32 -> Binaryen.float32
  F64 -> Binaryen.float64

marshalValueTypes :: [ValueType] -> IO Binaryen.Type
marshalValueTypes [] = pure Binaryen.none
marshalValueTypes [vt] = pure $ marshalValueType vt
marshalValueTypes vts = flip runContT pure $ do
  (vts', vtl) <- marshalV $ map marshalValueType vts
  lift $ Binaryen.Type.create vts' (fromIntegral vtl)

marshalReturnTypes :: [ValueType] -> IO Binaryen.Type
marshalReturnTypes vts = case vts of
  [] -> pure Binaryen.none
  [vt] -> pure $ marshalValueType vt
  _ ->
    fail $
      "binaryen doesn't support multi-value yet: failed to marshal "
        <> show vts

marshalUnaryOp :: UnaryOp -> Binaryen.Op
marshalUnaryOp op = case op of
  ClzInt32 -> Binaryen.clzInt32
  CtzInt32 -> Binaryen.ctzInt32
  PopcntInt32 -> Binaryen.popcntInt32
  NegFloat32 -> Binaryen.negFloat32
  AbsFloat32 -> Binaryen.absFloat32
  CeilFloat32 -> Binaryen.ceilFloat32
  FloorFloat32 -> Binaryen.floorFloat32
  TruncFloat32 -> Binaryen.truncFloat32
  NearestFloat32 -> Binaryen.nearestFloat32
  SqrtFloat32 -> Binaryen.sqrtFloat32
  EqZInt32 -> Binaryen.eqZInt32
  ClzInt64 -> Binaryen.clzInt64
  CtzInt64 -> Binaryen.ctzInt64
  PopcntInt64 -> Binaryen.popcntInt64
  NegFloat64 -> Binaryen.negFloat64
  AbsFloat64 -> Binaryen.absFloat64
  CeilFloat64 -> Binaryen.ceilFloat64
  FloorFloat64 -> Binaryen.floorFloat64
  TruncFloat64 -> Binaryen.truncFloat64
  NearestFloat64 -> Binaryen.nearestFloat64
  SqrtFloat64 -> Binaryen.sqrtFloat64
  EqZInt64 -> Binaryen.eqZInt64
  ExtendSInt32 -> Binaryen.extendSInt32
  ExtendUInt32 -> Binaryen.extendUInt32
  WrapInt64 -> Binaryen.wrapInt64
  TruncSFloat32ToInt32 -> Binaryen.truncSFloat32ToInt32
  TruncSFloat32ToInt64 -> Binaryen.truncSFloat32ToInt64
  TruncUFloat32ToInt32 -> Binaryen.truncUFloat32ToInt32
  TruncUFloat32ToInt64 -> Binaryen.truncUFloat32ToInt64
  TruncSFloat64ToInt32 -> Binaryen.truncSFloat64ToInt32
  TruncSFloat64ToInt64 -> Binaryen.truncSFloat64ToInt64
  TruncUFloat64ToInt32 -> Binaryen.truncUFloat64ToInt32
  TruncUFloat64ToInt64 -> Binaryen.truncUFloat64ToInt64
  ReinterpretFloat32 -> Binaryen.reinterpretFloat32
  ReinterpretFloat64 -> Binaryen.reinterpretFloat64
  ConvertSInt32ToFloat32 -> Binaryen.convertSInt32ToFloat32
  ConvertSInt32ToFloat64 -> Binaryen.convertSInt32ToFloat64
  ConvertUInt32ToFloat32 -> Binaryen.convertUInt32ToFloat32
  ConvertUInt32ToFloat64 -> Binaryen.convertUInt32ToFloat64
  ConvertSInt64ToFloat32 -> Binaryen.convertSInt64ToFloat32
  ConvertSInt64ToFloat64 -> Binaryen.convertSInt64ToFloat64
  ConvertUInt64ToFloat32 -> Binaryen.convertUInt64ToFloat32
  ConvertUInt64ToFloat64 -> Binaryen.convertUInt64ToFloat64
  PromoteFloat32 -> Binaryen.promoteFloat32
  DemoteFloat64 -> Binaryen.demoteFloat64
  ReinterpretInt32 -> Binaryen.reinterpretInt32
  ReinterpretInt64 -> Binaryen.reinterpretInt64

marshalBinaryOp :: BinaryOp -> Binaryen.Op
marshalBinaryOp op = case op of
  AddInt32 -> Binaryen.addInt32
  SubInt32 -> Binaryen.subInt32
  MulInt32 -> Binaryen.mulInt32
  DivSInt32 -> Binaryen.divSInt32
  DivUInt32 -> Binaryen.divUInt32
  RemSInt32 -> Binaryen.remSInt32
  RemUInt32 -> Binaryen.remUInt32
  AndInt32 -> Binaryen.andInt32
  OrInt32 -> Binaryen.orInt32
  XorInt32 -> Binaryen.xorInt32
  ShlInt32 -> Binaryen.shlInt32
  ShrUInt32 -> Binaryen.shrUInt32
  ShrSInt32 -> Binaryen.shrSInt32
  RotLInt32 -> Binaryen.rotLInt32
  RotRInt32 -> Binaryen.rotRInt32
  EqInt32 -> Binaryen.eqInt32
  NeInt32 -> Binaryen.neInt32
  LtSInt32 -> Binaryen.ltSInt32
  LtUInt32 -> Binaryen.ltUInt32
  LeSInt32 -> Binaryen.leSInt32
  LeUInt32 -> Binaryen.leUInt32
  GtSInt32 -> Binaryen.gtSInt32
  GtUInt32 -> Binaryen.gtUInt32
  GeSInt32 -> Binaryen.geSInt32
  GeUInt32 -> Binaryen.geUInt32
  AddInt64 -> Binaryen.addInt64
  SubInt64 -> Binaryen.subInt64
  MulInt64 -> Binaryen.mulInt64
  DivSInt64 -> Binaryen.divSInt64
  DivUInt64 -> Binaryen.divUInt64
  RemSInt64 -> Binaryen.remSInt64
  RemUInt64 -> Binaryen.remUInt64
  AndInt64 -> Binaryen.andInt64
  OrInt64 -> Binaryen.orInt64
  XorInt64 -> Binaryen.xorInt64
  ShlInt64 -> Binaryen.shlInt64
  ShrUInt64 -> Binaryen.shrUInt64
  ShrSInt64 -> Binaryen.shrSInt64
  RotLInt64 -> Binaryen.rotLInt64
  RotRInt64 -> Binaryen.rotRInt64
  EqInt64 -> Binaryen.eqInt64
  NeInt64 -> Binaryen.neInt64
  LtSInt64 -> Binaryen.ltSInt64
  LtUInt64 -> Binaryen.ltUInt64
  LeSInt64 -> Binaryen.leSInt64
  LeUInt64 -> Binaryen.leUInt64
  GtSInt64 -> Binaryen.gtSInt64
  GtUInt64 -> Binaryen.gtUInt64
  GeSInt64 -> Binaryen.geSInt64
  GeUInt64 -> Binaryen.geUInt64
  AddFloat32 -> Binaryen.addFloat32
  SubFloat32 -> Binaryen.subFloat32
  MulFloat32 -> Binaryen.mulFloat32
  DivFloat32 -> Binaryen.divFloat32
  CopySignFloat32 -> Binaryen.copySignFloat32
  MinFloat32 -> Binaryen.minFloat32
  MaxFloat32 -> Binaryen.maxFloat32
  EqFloat32 -> Binaryen.eqFloat32
  NeFloat32 -> Binaryen.neFloat32
  LtFloat32 -> Binaryen.ltFloat32
  LeFloat32 -> Binaryen.leFloat32
  GtFloat32 -> Binaryen.gtFloat32
  GeFloat32 -> Binaryen.geFloat32
  AddFloat64 -> Binaryen.addFloat64
  SubFloat64 -> Binaryen.subFloat64
  MulFloat64 -> Binaryen.mulFloat64
  DivFloat64 -> Binaryen.divFloat64
  CopySignFloat64 -> Binaryen.copySignFloat64
  MinFloat64 -> Binaryen.minFloat64
  MaxFloat64 -> Binaryen.maxFloat64
  EqFloat64 -> Binaryen.eqFloat64
  NeFloat64 -> Binaryen.neFloat64
  LtFloat64 -> Binaryen.ltFloat64
  LeFloat64 -> Binaryen.leFloat64
  GtFloat64 -> Binaryen.gtFloat64
  GeFloat64 -> Binaryen.geFloat64

marshalFunctionType :: FunctionType -> IO (Binaryen.Type, Binaryen.Type)
marshalFunctionType FunctionType {..} = do
  pt <- marshalValueTypes paramTypes
  rt <- marshalValueTypes returnTypes
  pure (pt, rt)

-- | Environment used during marshaling of Asterius' types to Binaryen.
data MarshalEnv
  = MarshalEnv
      { -- | Whether the tail call extension is on.
        envAreTailCallsOn :: Bool,
        -- | The symbol map for the current module.
        envSymbolMap :: SM.SymbolMap Int64,
        -- | The current module reference.
        envModuleRef :: Binaryen.Module
      }

type CodeGen a = ReaderT MarshalEnv IO a

-- | Check whether the tail call extension is on.
areTailCallsOn :: CodeGen Bool
areTailCallsOn = reader envAreTailCallsOn

-- | Retrieve the symbol map from the local environment.
askSymbolMap :: CodeGen (SM.SymbolMap Int64)
askSymbolMap = reader envSymbolMap

-- | Retrieve the reference to the current module.
askModuleRef :: CodeGen Binaryen.Module
askModuleRef = reader envModuleRef

marshalExpression :: Expression -> CodeGen Binaryen.Expression
marshalExpression e = case e of
  Block {..} -> do
    env <- ask
    m <- askModuleRef
    lift $ flip runContT pure $ do
      bs <- lift $ flip runReaderT env $ forM bodys marshalExpression
      (bsp, bl) <- marshalV bs
      np <- marshalBS name
      rts <- lift $ marshalReturnTypes blockReturnTypes
      lift $ Binaryen.block m np bsp (fromIntegral bl) rts
  If {..} -> do
    c <- marshalExpression condition
    t <- marshalExpression ifTrue
    f <- marshalMaybeExpression ifFalse
    m <- askModuleRef
    lift $ Binaryen.if_ m c t f
  Loop {..} -> do
    b <- marshalExpression body
    m <- askModuleRef
    lift $ flip runContT pure $ do
      np <- marshalBS name
      lift $ Binaryen.loop m np b
  Break {..} -> do
    c <- marshalMaybeExpression breakCondition
    m <- askModuleRef
    lift $ flip runContT pure $ do
      np <- marshalBS name
      lift $ Binaryen.break m np c (coerce nullPtr)
  Switch {..} -> do
    c <- marshalExpression condition
    m <- askModuleRef
    lift $ flip runContT pure $ do
      ns <- forM names marshalBS
      (nsp, nl) <- marshalV ns
      dn <- marshalBS defaultName
      lift $ Binaryen.switch m nsp (fromIntegral nl) dn c (coerce nullPtr)
  Call {..} -> do
    sym_map <- askSymbolMap
    if  | target `SM.member` sym_map ->
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
              lift $ Binaryen.call m tp ops (fromIntegral osl) rts
        | ("__asterius_barf_" <> target) `SM.member` sym_map ->
          marshalExpression $ barf target callReturnTypes
        | otherwise -> do
          m <- askModuleRef
          lift $ Binaryen.Expression.unreachable m
  CallImport {..} -> do
    os <- forM operands marshalExpression
    m <- askModuleRef
    lift $ flip runContT pure $ do
      (ops, osl) <- marshalV os
      tp <- marshalBS target'
      rts <- lift $ marshalReturnTypes callImportReturnTypes
      lift $ Binaryen.call m tp ops (fromIntegral osl) rts
  CallIndirect {..} -> do
    t <- marshalExpression indirectTarget
    os <- forM operands marshalExpression
    m <- askModuleRef
    lift $ flip runContT pure $ do
      (ops, osl) <- marshalV os
      lift $ do
        (pt, rt) <- marshalFunctionType functionType
        Binaryen.callIndirect m t ops (fromIntegral osl) pt rt
  GetLocal {..} -> do
    m <- askModuleRef
    lift $ Binaryen.localGet m (coerce index) $ marshalValueType valueType
  SetLocal {..} -> do
    v <- marshalExpression value
    m <- askModuleRef
    lift $ Binaryen.localSet m (coerce index) v
  TeeLocal {..} -> do
    v <- marshalExpression value
    m <- askModuleRef
    lift $ Binaryen.localTee m (coerce index) v $ marshalValueType valueType
  Load {..} -> do
    p <- marshalExpression ptr
    m <- askModuleRef
    lift $
      Binaryen.load
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
    lift $ Binaryen.store m bytes offset 1 p v (marshalValueType valueType)
  ConstI32 x -> do
    m <- askModuleRef
    lift $ Binaryen.constInt32 m x
  ConstI64 x -> do
    m <- askModuleRef
    lift $ Binaryen.constInt64 m x
  ConstF32 x -> do
    m <- askModuleRef
    lift $ Binaryen.constFloat32 m x
  ConstF64 x -> do
    m <- askModuleRef
    lift $ Binaryen.constFloat64 m x
  Unary {..} -> do
    x <- marshalExpression operand0
    m <- askModuleRef
    lift $ Binaryen.unary m (marshalUnaryOp unaryOp) x
  Binary {..} -> do
    x <- marshalExpression operand0
    y <- marshalExpression operand1
    m <- askModuleRef
    lift $ Binaryen.binary m (marshalBinaryOp binaryOp) x y
  Drop {..} -> do
    x <- marshalExpression dropValue
    m <- askModuleRef
    lift $ Binaryen.drop m x
  ReturnCall {..} -> areTailCallsOn >>= \case
    -- Case 1: Tail calls are on
    True -> do
      m <- askModuleRef
      lift $ flip runContT pure $ do
        dst <- marshalBS (entityName returnCallTarget64)
        lift $ Binaryen.returnCall m dst nullPtr 0 Binaryen.none
    -- Case 2: Tail calls are off
    False -> do
      sym_map <- askSymbolMap
      case SM.lookup returnCallTarget64 sym_map of
        Just t -> do
          s <-
            marshalExpression
              Store
                { bytes = 8,
                  offset = 0,
                  ptr =
                    ConstI32
                      $ fromIntegral
                      $ (sym_map SM.! "__asterius_pc")
                        .&. 0xFFFFFFFF,
                  value = ConstI64 t,
                  valueType = I64
                }
          m <- askModuleRef
          lift $ flip runContT pure $ do
            r <- lift $ Binaryen.return m (coerce nullPtr)
            (arr, _) <- marshalV [s, r]
            lift $ Binaryen.block m (coerce nullPtr) arr 2 Binaryen.none
        Nothing -> marshalExpression $ barf returnCallTarget64 []
  ReturnCallIndirect {..} -> areTailCallsOn >>= \case
    -- Case 1: Tail calls are on
    True -> do
      t <-
        marshalExpression
          Unary {unaryOp = WrapInt64, operand0 = returnCallIndirectTarget64}
      m <- askModuleRef
      lift $
        Binaryen.returnCallIndirect
          m
          t
          nullPtr
          0
          Binaryen.none
          Binaryen.none
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
                  $ (sym_map SM.! "__asterius_pc")
                    .&. 0xFFFFFFFF,
              value = returnCallIndirectTarget64,
              valueType = I64
            }
      m <- askModuleRef
      lift $ flip runContT pure $ do
        r <- lift $ Binaryen.return m (coerce nullPtr)
        (arr, _) <- marshalV [s, r]
        lift $ Binaryen.block m nullPtr arr 2 Binaryen.none
  Nop -> do
    m <- askModuleRef
    lift $ Binaryen.nop m
  Unreachable -> do
    m <- askModuleRef
    lift $ Binaryen.Expression.unreachable m
  CFG {..} -> relooperRun graph
  Symbol {..} -> do
    sym_map <- askSymbolMap
    m <- askModuleRef
    case SM.lookup unresolvedSymbol sym_map of
      Just x -> lift $ Binaryen.constInt64 m $ x + fromIntegral symbolOffset
      _
        | ("__asterius_barf_" <> unresolvedSymbol) `SM.member` sym_map ->
          marshalExpression $ barf unresolvedSymbol [I64]
        | otherwise ->
          lift $ Binaryen.constInt64 m invalidAddress
  -- Unsupported expressions
  UnresolvedGetLocal {} -> lift $ throwIO $ UnsupportedExpression e
  UnresolvedSetLocal {} -> lift $ throwIO $ UnsupportedExpression e
  Barf {} -> lift $ throwIO $ UnsupportedExpression e

marshalMaybeExpression :: Maybe Expression -> CodeGen Binaryen.Expression
marshalMaybeExpression x = case x of
  Just e -> marshalExpression e
  _ -> pure (coerce nullPtr)

marshalFunction ::
  BS.ByteString ->
  (Binaryen.Type, Binaryen.Type) ->
  Function ->
  CodeGen Binaryen.Function
marshalFunction k (pt, rt) Function {..} = do
  env <- ask
  m <- askModuleRef
  lift $ flip runContT pure $ do
    b <- lift $ flip runReaderT env $ marshalExpression body
    (vtp, vtl) <- marshalV $ map marshalValueType varTypes
    np <- marshalBS k
    lift $ Binaryen.addFunction m np pt rt vtp (fromIntegral vtl) b

marshalFunctionImport ::
  Binaryen.Module ->
  (Binaryen.Type, Binaryen.Type) ->
  FunctionImport ->
  IO ()
marshalFunctionImport m (pt, rt) FunctionImport {..} = flip runContT pure $ do
  inp <- marshalBS internalName
  emp <- marshalBS externalModuleName
  ebp <- marshalBS externalBaseName
  lift $ Binaryen.addFunctionImport m inp emp ebp pt rt

marshalFunctionExport ::
  Binaryen.Module -> FunctionExport -> IO Binaryen.Export
marshalFunctionExport m FunctionExport {..} = flip runContT pure $ do
  inp <- marshalBS internalName
  enp <- marshalBS externalName
  lift $ Binaryen.addFunctionExport m inp enp

marshalFunctionTable :: Binaryen.Module -> Int -> FunctionTable -> IO ()
marshalFunctionTable m tbl_slots FunctionTable {..} = flip runContT pure $ do
  func_name_ptrs <- for tableFunctionNames marshalBS
  (fnp, fnl) <- marshalV func_name_ptrs
  lift $ do
    o <- Binaryen.constInt32 m (fromIntegral tableOffset)
    Binaryen.setFunctionTable
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
      Binaryen.setMemory
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

marshalTableImport :: Binaryen.Module -> TableImport -> IO ()
marshalTableImport m TableImport {..} = flip runContT pure $ do
  inp <- marshalBS "0"
  emp <- marshalBS externalModuleName
  ebp <- marshalBS externalBaseName
  lift $ Binaryen.addTableImport m inp emp ebp

marshalMemoryImport :: Binaryen.Module -> MemoryImport -> IO ()
marshalMemoryImport m MemoryImport {..} = flip runContT pure $ do
  inp <- marshalBS "0"
  emp <- marshalBS externalModuleName
  ebp <- marshalBS externalBaseName
  lift $ Binaryen.addMemoryImport m inp emp ebp 0

marshalModule ::  -- TODO: Parallelize (see how to do it best)
  Bool -> SM.SymbolMap Int64 -> Module -> IO Binaryen.Module
marshalModule tail_calls sym_map hs_mod@Module {..} = do
  let fts = generateWasmFunctionTypeSet hs_mod
  m <- Binaryen.Module.create
  Binaryen.setFeatures m
    $ foldl1' (.|.)
    $ [Binaryen.tailCall | tail_calls]
      <> [Binaryen.mvp]
  ftps <- fmap M.fromList $ for (Set.toList fts) $ \ft -> do
    ftp <- marshalFunctionType ft
    pure (ft, ftp)
  let env =
        MarshalEnv
          { envAreTailCallsOn = tail_calls,
            envSymbolMap = sym_map,
            envModuleRef = m
          }
  parallelFor_ 1 (M.toList functionMap') $ \(k, f@Function {..}) -> -- TODO: Parameterize (and ensure allowed)
    flip runReaderT env $ marshalFunction k (ftps M.! functionType) f
  parallelFor_ 1 functionImports $ \fi@FunctionImport {..} -> -- TODO: Parameterize (and ensure allowed)
    marshalFunctionImport m (ftps M.! functionType) fi
  parallelFor_ 1 functionExports $ marshalFunctionExport m -- TODO: Parameterize (and ensure allowed)
  marshalFunctionTable m tableSlots functionTable
  marshalTableImport m tableImport
  flip runReaderT env $ marshalMemorySegments memoryMBlocks memorySegments
  marshalMemoryImport m memoryImport
  flip runContT pure $ do
    lim_segs <- marshalBS "limit-segments"
    (lim_segs_p, _) <- marshalV [lim_segs]
    lift $ Binaryen.Module.runPasses m lim_segs_p 1
  pure m

relooperAddBlock :: Binaryen.Relooper -> RelooperAddBlock -> CodeGen Binaryen.RelooperBlock
relooperAddBlock r ab = case ab of
  AddBlock {..} -> do
    c <- marshalExpression code
    lift $ Binaryen.addBlock r c
  AddBlockWithSwitch {..} -> do
    _code <- marshalExpression code
    _cond <- marshalExpression condition
    lift $ Binaryen.addBlockWithSwitch r _code _cond

relooperAddBranch ::
  M.Map BS.ByteString Binaryen.RelooperBlock ->
  BS.ByteString ->
  RelooperAddBranch ->
  CodeGen ()
relooperAddBranch bm k ab = case ab of
  AddBranch {..} -> do
    _cond <- marshalMaybeExpression addBranchCondition
    lift $ Binaryen.addBranch (bm M.! k) (bm M.! to) _cond (coerce nullPtr)
  AddBranchForSwitch {..} -> lift $ flip runContT pure $ do
    (idp, idn) <- marshalV indexes
    lift $
      Binaryen.addBranchForSwitch
        (bm M.! k)
        (bm M.! to)
        (coerce idp)
        (fromIntegral idn)
        (coerce nullPtr)

relooperRun :: RelooperRun -> CodeGen Binaryen.Expression
relooperRun RelooperRun {..} = do
  m <- askModuleRef
  r <- lift $ Binaryen.Relooper.create m
  bpm <- fmap fromList $ for (M.toList blockMap) $ \(k, RelooperBlock {..}) ->
    do
      bp <- relooperAddBlock r addBlock
      pure (k, bp)
  for_ (M.toList blockMap) $ \(k, RelooperBlock {..}) ->
    forM_ addBranches $ relooperAddBranch bpm k
  lift $ Binaryen.renderAndDispose r (bpm M.! entry) (coerce labelHelper)

serializeModule :: Binaryen.Module -> IO BS.ByteString
serializeModule m = alloca $ \(buf_p :: Ptr (Ptr ())) ->
  alloca $ \(len_p :: Ptr CSize) -> alloca $ \(src_map_p :: Ptr (Ptr CChar)) ->
    do
      Binaryen.allocateAndWriteMut m nullPtr buf_p len_p src_map_p
      buf <- peek buf_p
      len <- peek len_p
      BS.unsafePackMallocCStringLen (castPtr buf, fromIntegral len)

serializeModuleSExpr :: Binaryen.Module -> IO BS.ByteString
serializeModuleSExpr m =
  Binaryen.allocateAndWriteText m >>= BS.unsafePackCString

setColorsEnabled :: Bool -> IO ()
setColorsEnabled b = Binaryen.setColorsEnabled . toEnum . fromEnum $ b
