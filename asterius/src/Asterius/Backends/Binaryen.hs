{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
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
  )
where

import Asterius.Backends.Binaryen.CheckOverlapDataSegment
import qualified Asterius.Internals.Arena as A
import Asterius.Internals.Barf
import Asterius.Internals.MagicNumber
import Asterius.Internals.Marshal
import Asterius.Passes.CCall
import Asterius.Passes.Relooper
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Asterius.TypesConv
import qualified Binaryen
import qualified Binaryen.Export
import qualified Binaryen.Expression
import qualified Binaryen.Expression as Binaryen
import qualified Binaryen.ExternalKind as Binaryen
import qualified Binaryen.Features as Binaryen
import qualified Binaryen.Function
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
import Data.Maybe
import qualified Data.Set as Set
import Data.Traversable
import Foreign hiding
  ( void,
    withPool,
  )
import Foreign.C
import GHC.Exts
import Asterius.JSGen.Wizer

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

marshalValueTypes :: [ValueType] -> CodeGen Binaryen.Type
marshalValueTypes [] = pure Binaryen.none
marshalValueTypes [vt] = pure $ marshalValueType vt
marshalValueTypes vts = do
  a <- askArena
  lift $ do
    (vts', vtl) <- marshalV a $ map marshalValueType vts
    Binaryen.Type.create vts' (fromIntegral vtl)

marshalReturnTypes :: [ValueType] -> CodeGen Binaryen.Type
marshalReturnTypes vts = case vts of
  [] -> pure Binaryen.none
  [vt] -> pure $ marshalValueType vt
  _ ->
    fail $
      "binaryen doesn't support multi-value yet: failed to marshal "
        <> show vts

marshalMutability :: Mutability -> Int8
marshalMutability = \case
  Immutable -> 0
  Mutable -> 1

marshalGlobalType :: GlobalType -> (Binaryen.Type, Int8)
marshalGlobalType GlobalType {..} =
  (marshalValueType globalValueType, marshalMutability globalMutability)

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

marshalFunctionType :: FunctionType -> CodeGen (Binaryen.Type, Binaryen.Type)
marshalFunctionType FunctionType {..} = do
  pt <- marshalValueTypes paramTypes
  rt <- marshalValueTypes returnTypes
  pure (pt, rt)

-- | Environment used during marshaling of Asterius' types to Binaryen.
data MarshalEnv = MarshalEnv
  { -- | Info about exported functions in the libc module, indexed by external name.
    envLibCFuncInfo :: M.Map BS.ByteString (BS.ByteString, FunctionType),
    -- | Internal names of exported functions in the libc module.
    envLibCFuncNames :: Set.Set BS.ByteString,
    -- | The 'A.Arena' for allocating temporary buffers.
    envArena :: A.Arena,
    -- | Whether the @verbose_err@ extension is on.
    envIsVerboseErrOn :: Bool,
    -- | The offset map for the current module (statics).
    envStaticsOffsetMap :: SM.SymbolMap Word32,
    -- | The offset map for the current module (functions).
    envFunctionsOffsetMap :: SM.SymbolMap Word32,
    envMemoryBase :: Word32,
    -- | The current module reference.
    envModuleRef :: Binaryen.Module
  }

type CodeGen a = ReaderT MarshalEnv IO a

-- | Retrieve the 'A.Arena'.
askArena :: CodeGen A.Arena
askArena = reader envArena

-- | Check whether the @verbose_err@ extension is on.
isVerboseErrOn :: CodeGen Bool
isVerboseErrOn = reader envIsVerboseErrOn

-- | Retrieve the offset map from the local environment (statics).
askStaticsOffsetMap :: CodeGen (SM.SymbolMap Word32)
askStaticsOffsetMap = reader envStaticsOffsetMap

-- | Retrieve the offset map from the local environment (functions).
askFunctionsOffsetMap :: CodeGen (SM.SymbolMap Word32)
askFunctionsOffsetMap = reader envFunctionsOffsetMap

askMemoryBase :: CodeGen Word32
askMemoryBase = reader envMemoryBase

-- | Retrieve the reference to the current module.
askModuleRef :: CodeGen Binaryen.Module
askModuleRef = reader envModuleRef

marshalExpression :: Expression -> CodeGen Binaryen.Expression
marshalExpression e' = do
 libc_func_info <- asks envLibCFuncInfo
 let e = handleCCall libc_func_info e'
 case e of
  Block {..} -> do
    bs <- forM bodys marshalExpression
    rts <- marshalReturnTypes blockReturnTypes
    m <- askModuleRef
    a <- askArena
    lift $ do
      (bsp, bl) <- marshalV a bs
      np <- marshalBS a name
      Binaryen.block m np bsp (fromIntegral bl) rts
  If {..} -> do
    c <- marshalExpression condition
    t <- marshalExpression ifTrue
    f <- marshalMaybeExpression ifFalse
    m <- askModuleRef
    lift $ Binaryen.if_ m c t f
  Loop {..} -> do
    b <- marshalExpression body
    m <- askModuleRef
    a <- askArena
    lift $ do
      np <- marshalBS a name
      Binaryen.loop m np b
  Break {..} -> do
    c <- marshalMaybeExpression breakCondition
    m <- askModuleRef
    a <- askArena
    lift $ do
      np <- marshalBS a name
      Binaryen.break m np c (coerce nullPtr)
  Switch {..} -> do
    c <- marshalExpression condition
    m <- askModuleRef
    a <- askArena
    lift $ do
      ns <- forM names $ marshalBS a
      (nsp, nl) <- marshalV a ns
      dn <- marshalBS a defaultName
      Binaryen.switch m nsp (fromIntegral nl) dn c (coerce nullPtr)
  Call {..} -> do
    verbose_err <- isVerboseErrOn
    fn_off_map <- askFunctionsOffsetMap
    libc_func_names <- asks envLibCFuncNames
    if  | target `SM.member` fn_off_map || entityName target `Set.member` libc_func_names -> do
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
          rts <- marshalReturnTypes callReturnTypes
          m <- askModuleRef
          a <- askArena
          lift $ do
            (ops, osl) <- marshalV a os
            tp <- marshalBS a (entityName target)
            Binaryen.call m tp ops (fromIntegral osl) rts
        | verbose_err ->
          marshalExpression $
            barf (entityName target) callReturnTypes
        | otherwise -> do
          m <- askModuleRef
          lift $ Binaryen.Expression.unreachable m
  CallImport {..} -> do
    os <- forM operands marshalExpression
    rts <- marshalReturnTypes callImportReturnTypes
    m <- askModuleRef
    a <- askArena
    lift $ do
      (ops, osl) <- marshalV a os
      tp <- marshalBS a target'
      Binaryen.call m tp ops (fromIntegral osl) rts
  CallIndirect {..} -> do
    t <- marshalExpression indirectTarget
    os <- forM operands marshalExpression
    (pt, rt) <- marshalFunctionType functionType
    m <- askModuleRef
    a <- askArena
    lift $ do
      (ops, osl) <- marshalV a os
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
  GetGlobal {..} -> do
    m <- askModuleRef
    a <- askArena
    lift $ do
      gbl <- marshalBS a (entityName globalSymbol)
      Binaryen.globalGet m gbl $ marshalValueType valueType
  SetGlobal {..} -> do
    v <- marshalExpression value
    m <- askModuleRef
    a <- askArena
    lift $ do
      gbl <- marshalBS a (entityName globalSymbol)
      Binaryen.globalSet m gbl v
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
  ReturnCall {..} -> do
      fn_off_map <- askFunctionsOffsetMap
      case SM.lookup returnCallTarget fn_off_map of
        Just off -> do
          s <-
            marshalExpression
              SetGlobal
                { globalSymbol = "__asterius_pc",
                  value = ConstI32 $ mkStaticFunctionAddress off
                }
          m <- askModuleRef
          a <- askArena
          lift $ do
            r <- Binaryen.return m (coerce nullPtr)
            (arr, _) <- marshalV a [s, r]
            Binaryen.block m (coerce nullPtr) arr 2 Binaryen.none
        Nothing -> marshalExpression $ barf (entityName returnCallTarget) []
  ReturnCallIndirect {..} -> do
      s <-
        marshalExpression
          SetGlobal
            { globalSymbol = "__asterius_pc",
              value = returnCallIndirectTarget
            }
      m <- askModuleRef
      a <- askArena
      lift $ do
        r <- Binaryen.return m (coerce nullPtr)
        (arr, _) <- marshalV a [s, r]
        Binaryen.block m nullPtr arr 2 Binaryen.none
  Nop -> do
    m <- askModuleRef
    lift $ Binaryen.nop m
  Unreachable -> do
    m <- askModuleRef
    lift $ Binaryen.Expression.unreachable m
  CFG {..} -> marshalExpression $ relooper graph
  Symbol {..} -> do
    verbose_err <- isVerboseErrOn
    ss_off_map <- askStaticsOffsetMap
    fn_off_map <- askFunctionsOffsetMap
    memory_base <- askMemoryBase
    m <- askModuleRef
    if  | Just off <- SM.lookup unresolvedSymbol ss_off_map ->
          marshalExpression $
            ConstI32 $ mkStaticDataAddress memory_base $ off + fromIntegral symbolOffset
        | Just off <- SM.lookup unresolvedSymbol fn_off_map ->
          marshalExpression $
            ConstI32 $ mkStaticFunctionAddress $ off + fromIntegral symbolOffset
        | verbose_err ->
          marshalExpression $ barf (entityName unresolvedSymbol) [I64]
        | otherwise ->
          lift $ Binaryen.constInt32 m invalidAddress
  Barf {..} -> do
    verbose_err <- isVerboseErrOn
    if verbose_err
      then marshalExpression $ barf barfMessage barfReturnTypes
      else do
        m <- askModuleRef
        lift $ Binaryen.Expression.unreachable m
  -- Unsupported expressions
  UnresolvedGetLocal {} -> lift $ throwIO $ UnsupportedExpression e
  UnresolvedSetLocal {} -> lift $ throwIO $ UnsupportedExpression e

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
  b <- marshalExpression body
  m <- askModuleRef
  a <- askArena
  lift $ do
    (vtp, vtl) <- marshalV a $ map marshalValueType varTypes
    np <- marshalBS a k
    Binaryen.addFunction m np pt rt vtp (fromIntegral vtl) b

marshalFunctionImport ::
  Binaryen.Module ->
  (Binaryen.Type, Binaryen.Type) ->
  FunctionImport ->
  CodeGen ()
marshalFunctionImport m (pt, rt) FunctionImport {..} = do
  a <- askArena
  lift $ do
    inp <- marshalBS a internalName
    emp <- marshalBS a externalModuleName
    ebp <- marshalBS a externalBaseName
    Binaryen.addFunctionImport m inp emp ebp pt rt

marshalFunctionExport ::
  Binaryen.Module -> FunctionExport -> CodeGen Binaryen.Export
marshalFunctionExport m FunctionExport {..} = do
  a <- askArena
  lift $ do
    inp <- marshalBS a internalName
    enp <- marshalBS a externalName
    Binaryen.addFunctionExport m inp enp

marshalFunctionTable :: Binaryen.Module -> Int -> FunctionTable -> CodeGen ()
marshalFunctionTable m tbl_slots FunctionTable {..} = do
  a <- askArena
  o <- marshalExpression tableOffset
  lift $ do
    func_name_ptrs <- for tableFunctionNames $ marshalBS a
    (fnp, fnl) <- marshalV a func_name_ptrs
    Binaryen.setFunctionTable
      m
      (fromIntegral defaultTableBase + fromIntegral tbl_slots)
      (-1)
      fnp
      (fromIntegral fnl)
      o

marshalMemorySegments :: [DataSegment] -> CodeGen ()
marshalMemorySegments segs = do
  env <- ask
  m <- askModuleRef
  a <- askArena
  let segs_len = length segs
  lift $ do
    (seg_bufs, _) <- marshalV a =<< for segs (marshalBS a . content)
    (seg_passives, _) <- marshalV a $ replicate segs_len 0
    (seg_offsets, _) <-
      marshalV a
        =<< for
          segs
          ( \DataSegment {..} ->
              flip runReaderT env $ marshalExpression offset
          )
    (seg_sizes, _) <- marshalV a $ map (fromIntegral . BS.length . content) segs
    i <- c_BinaryenGetMemoryInitial m
    Binaryen.setMemory
      m
      i
      (-1)
      nullPtr
      seg_bufs
      seg_passives
      seg_offsets
      seg_sizes
      (fromIntegral segs_len)
      0

marshalTableImport :: Binaryen.Module -> TableImport -> CodeGen ()
marshalTableImport m TableImport {..} = do
  a <- askArena
  lift $ do
    inp <- marshalBS a "0"
    emp <- marshalBS a externalModuleName
    ebp <- marshalBS a externalBaseName
    Binaryen.addTableImport m inp emp ebp

marshalMemoryImport :: Binaryen.Module -> MemoryImport -> CodeGen ()
marshalMemoryImport m MemoryImport {..} = do
  a <- askArena
  lift $ do
    inp <- marshalBS a "0"
    emp <- marshalBS a externalModuleName
    ebp <- marshalBS a externalBaseName
    Binaryen.addMemoryImport m inp emp ebp 0

marshalGlobalImport :: Binaryen.Module -> GlobalImport -> CodeGen ()
marshalGlobalImport m GlobalImport {..} = do
  a <- askArena
  lift $ do
    inp <- marshalBS a internalName
    emp <- marshalBS a externalModuleName
    ebp <- marshalBS a externalBaseName
    let (ty, mut) = marshalGlobalType globalType
    Binaryen.addGlobalImport m inp emp ebp ty (fromIntegral mut)

marshalGlobalExport ::
  Binaryen.Module -> GlobalExport -> CodeGen Binaryen.Export
marshalGlobalExport m GlobalExport {..} = do
  a <- askArena
  lift $ do
    inp <- marshalBS a internalName
    enp <- marshalBS a externalName
    Binaryen.addGlobalExport m inp enp

marshalGlobal ::
  BS.ByteString -> Global -> CodeGen Binaryen.Global
marshalGlobal k Global {..} = do
  let (ty, mut) = marshalGlobalType globalType
  e <- marshalExpression globalInit
  m <- askModuleRef
  a <- askArena
  lift $ do
    ptr <- marshalBS a k
    Binaryen.addGlobal m ptr ty mut e

marshalModule ::
  Bool ->
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  Word32 ->
  Module ->
  IO Binaryen.Module
marshalModule verbose_err ss_off_map fn_off_map last_data_offset hs_mod@Module {..} = do
  (m, memory_base) <- do
    (bs, memory_base) <- wizer last_data_offset
    m <- BS.unsafeUseAsCStringLen bs $
        \(p, l) -> Binaryen.Module.read p (fromIntegral l)
    pure (m, memory_base)
  checkOverlapDataSegment m
  Binaryen.setFeatures m
    $ foldl1' (.|.) [Binaryen.mvp]
  A.with $ \a -> do
    libc_func_names <- binaryenModuleExportNames m
    for_ libc_func_names $
      \(_, ext_name) ->
        when (ext_name `elem` ["_initialize"]) $ do
          p <- marshalBS a ext_name
          Binaryen.removeExport m p
    libc_func_info <- fmap M.fromList $ for libc_func_names $ \(in_name, ext_name) -> (ext_name,) . (in_name,) <$> binaryenFunctionType m in_name
    let env =
          MarshalEnv
              { envLibCFuncInfo = libc_func_info,
              envLibCFuncNames = Set.fromList $ map fst libc_func_names,
              envArena = a,
              envIsVerboseErrOn = verbose_err,
              envStaticsOffsetMap = ss_off_map,
              envFunctionsOffsetMap = fn_off_map,
              envMemoryBase = memory_base,
              envModuleRef = m
            }
        fts = generateWasmFunctionTypeSet hs_mod
    flip runReaderT env $ do
      ftps <- fmap M.fromList $ for (Set.toList fts) $ \ft -> do
        ftp <- marshalFunctionType ft
        pure (ft, ftp)
      for_ (M.toList functionMap') $
        \(k, f@Function {..}) -> marshalFunction k (ftps M.! functionType) f
      forM_ functionImports $ \fi@FunctionImport {..} ->
        marshalFunctionImport m (ftps M.! functionType) fi
      forM_ functionExports $ marshalFunctionExport m
      forM_ (SM.toList globalMap) $
        \(k, global) -> marshalGlobal (entityName k) global
      forM_ globalImports $ marshalGlobalImport m
      forM_ globalExports $ marshalGlobalExport m
      marshalFunctionTable m tableSlots functionTable
      case tableImport of
        Just tbl_import -> marshalTableImport m tbl_import
        _ -> pure ()
      marshalMemorySegments memorySegments
      lift $ checkOverlapDataSegment m
      case memoryImport of
        Just mem_import -> marshalMemoryImport m mem_import
        _ -> pure ()
  pure m

relooperAddBlock ::
  Binaryen.Relooper -> RelooperAddBlock -> CodeGen Binaryen.RelooperBlock
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
  AddBranchForSwitch {..} -> do
    a <- askArena
    lift $ do
      (idp, idn) <- marshalV a indexes
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
  for_ (M.toList blockMap) $
    \(k, RelooperBlock {..}) -> forM_ addBranches $ relooperAddBranch bpm k
  lift $ Binaryen.renderAndDispose r (bpm M.! entry) (coerce labelHelper)

serializeModule :: Binaryen.Module -> IO BS.ByteString
serializeModule m = alloca $ \(buf_p :: Ptr (Ptr ())) ->
  alloca $ \(len_p :: Ptr CSize) -> alloca $ \(src_map_p :: Ptr (Ptr CChar)) ->
    do
      Binaryen.allocateAndWriteMut m nullPtr buf_p len_p src_map_p
      buf <- peek buf_p
      len <- peek len_p
      BS.unsafePackMallocCStringLen (castPtr buf, fromIntegral len)

binaryenTypeToValueType :: Binaryen.Type -> ValueType
binaryenTypeToValueType ty =
  case lookup
    ty
    [ (Binaryen.int32, I32),
      (Binaryen.int64, I64),
      (Binaryen.float32, F32),
      (Binaryen.float64, F64)
    ] of
    Just r -> r
    _ -> error "binaryenTypeToValueType"

binaryenExpandType :: Binaryen.Type -> IO [Binaryen.Type]
binaryenExpandType tys = do
  n <- fromIntegral <$> Binaryen.Type.arity tys
  case n of
    0 -> pure []
    _ -> allocaArray n $ \p -> do
            Binaryen.Type.expand tys p
            peekArray n p

binaryenFunctionType :: Binaryen.Module -> BS.ByteString -> IO FunctionType
binaryenFunctionType m in_name = do
  f <- BS.useAsCString in_name $ \p -> Binaryen.getFunction m p
  args_tys <-
    (map binaryenTypeToValueType <$>)
      . binaryenExpandType
      =<< Binaryen.Function.getParams f
  res_tys <-
    (map binaryenTypeToValueType <$>)
      . binaryenExpandType
      =<< Binaryen.Function.getResults f
  pure FunctionType {paramTypes = args_tys, returnTypes = res_tys}

binaryenModuleExportNames ::
  Binaryen.Module -> IO [(BS.ByteString, BS.ByteString)]
binaryenModuleExportNames m = do
  n <- Binaryen.getNumExports m
  fmap catMaybes $
    for [0 .. n - 1] $ \i -> do
      e <- Binaryen.getExportByIndex m (fromIntegral i)
      k <- Binaryen.Export.getKind e
      if k == Binaryen.externalFunction
        then do
          in_name <- BS.packCString =<< Binaryen.Export.getValue e
          ext_name <- BS.packCString =<< Binaryen.Export.getName e
          pure $ Just (in_name, ext_name)
        else pure Nothing

foreign import ccall unsafe "BinaryenGetMemoryInitial" c_BinaryenGetMemoryInitial :: Binaryen.Module -> IO Binaryen.Index
