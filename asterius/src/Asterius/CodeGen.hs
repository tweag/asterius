{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.CodeGen
  ( CodeGen,
    runCodeGen,
    marshalHaskellIR,
    marshalCmmIR,
    marshalRawCmm,
  )
where

import Asterius.Builtins
import Asterius.EDSL
import Asterius.Internals
import Asterius.Internals.Name
import Asterius.Internals.PrettyShow
import Asterius.Passes.All
import Asterius.Passes.GlobalRegs
import Asterius.Resolve
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Asterius.TypesConv
import qualified CLabel as GHC
import qualified Cmm as GHC
import qualified CmmSwitch as GHC
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Monoid
import Data.String
import Data.Traversable
import Foreign
import GHC.Fingerprint
import qualified GhcPlugins as GHC
import qualified Hoopl.Block as GHC
import qualified Hoopl.Graph as GHC
import qualified Hoopl.Label as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Orphans.Show
  (
  )
import MonadUtils (mapAccumLM)
import Stream (Stream)
import qualified Stream
import qualified Unique as GHC

type CodeGenContext = (GHC.DynFlags, String)

newtype CodeGen a
  = CodeGen (ReaderT CodeGenContext IO a)
  deriving (Functor, Applicative, Monad, MonadReader CodeGenContext, MonadIO, MonadThrow, MonadCatch)

{-# INLINEABLE runCodeGen #-}
runCodeGen ::
  CodeGen a ->
  GHC.DynFlags ->
  GHC.Module ->
  IO a
runCodeGen (CodeGen m) dflags def_mod =
  runReaderT m (dflags, asmPpr dflags def_mod <> "_")

marshalCLabel :: GHC.CLabel -> CodeGen EntitySymbol
marshalCLabel clbl = do
  (dflags, def_mod_prefix) <- ask
  pure $ fromString $
    if GHC.externallyVisibleCLabel clbl
      then asmPpr dflags clbl
      else def_mod_prefix <> asmPpr dflags clbl

marshalLabel :: GHC.Label -> CodeGen BS.ByteString
marshalLabel lbl = do
  (dflags, _) <- ask
  pure $ fromString $ asmPpr dflags lbl

marshalCmmType :: GHC.CmmType -> CodeGen ValueType
marshalCmmType t
  | GHC.b8
      `GHC.cmmEqType_ignoring_ptrhood` t
      || GHC.b16
      `GHC.cmmEqType_ignoring_ptrhood` t
      || GHC.b32
      `GHC.cmmEqType_ignoring_ptrhood` t =
    pure I32
  | GHC.b64 `GHC.cmmEqType_ignoring_ptrhood` t =
    pure I64
  | GHC.f32 `GHC.cmmEqType_ignoring_ptrhood` t =
    pure F32
  | GHC.f64 `GHC.cmmEqType_ignoring_ptrhood` t =
    pure F64
  | otherwise =
    throwM $ UnsupportedCmmType $ showBS t

dispatchCmmWidth :: GHC.Width -> a -> a -> CodeGen a
dispatchCmmWidth w r32 = dispatchAllCmmWidth w r32 r32 r32

dispatchAllCmmWidth :: GHC.Width -> a -> a -> a -> a -> CodeGen a
dispatchAllCmmWidth w r8 r16 r32 r64 = case w of
  GHC.W8 -> pure r8
  GHC.W16 -> pure r16
  GHC.W32 -> pure r32
  GHC.W64 -> pure r64
  _ -> throwM $ UnsupportedCmmWidth $ showBS w

marshalForeignHint :: GHC.ForeignHint -> FFIHint
marshalForeignHint GHC.NoHint = NoHint
marshalForeignHint GHC.AddrHint = AddrHint
marshalForeignHint GHC.SignedHint = SignedHint

marshalCmmStatic :: GHC.CmmStatic -> CodeGen AsteriusStatic
marshalCmmStatic st = case st of
  GHC.CmmStaticLit lit -> case lit of
    GHC.CmmInt x w ->
      Serialized
        <$> dispatchAllCmmWidth
          w
          ( if x < 0
              then encodeStorable (fromIntegral x :: Int8)
              else encodeStorable (fromIntegral x :: Word8)
          )
          ( if x < 0
              then encodeStorable (fromIntegral x :: Int16)
              else encodeStorable (fromIntegral x :: Word16)
          )
          ( if x < 0
              then encodeStorable (fromIntegral x :: Int32)
              else encodeStorable (fromIntegral x :: Word32)
          )
          ( if x < 0
              then encodeStorable (fromIntegral x :: Int64)
              else encodeStorable (fromIntegral x :: Word64)
          )
    GHC.CmmFloat x w ->
      Serialized
        <$> dispatchCmmWidth
          w
          (encodeStorable (fromRational x :: Float))
          (encodeStorable (fromRational x :: Double))
    GHC.CmmLabel clbl -> flip SymbolStatic 0 <$> marshalCLabel clbl
    GHC.CmmLabelOff clbl o -> do
      sym <- marshalCLabel clbl
      pure $ SymbolStatic sym o
    _ -> throwM $ UnsupportedCmmLit $ showBS lit
  GHC.CmmUninitialised s -> pure $ Uninitialized s
  GHC.CmmString s -> pure $ Serialized $ s `BS.snoc` 0

marshalCmmSectionType :: EntitySymbol -> GHC.Section -> AsteriusStaticsType
marshalCmmSectionType _ (GHC.Section GHC.ReadOnlyData16 _) =
  error "ReadOnlyData16"
marshalCmmSectionType sym sec@(GHC.Section _ clbl)
  | GHC.isGcPtrLabel clbl = Closure
  | "_info" `BS.isSuffixOf` entityName sym = InfoTable
  | GHC.sectionProtection sec /= GHC.ReadWriteSection = ConstBytes
  | otherwise = Bytes

marshalCmmData ::
  EntitySymbol ->
  GHC.Section ->
  GHC.CmmStatics ->
  CodeGen AsteriusStatics
marshalCmmData sym sec (GHC.Statics _ ss) = do
  ass <- for ss marshalCmmStatic
  pure AsteriusStatics
    { staticsType = marshalCmmSectionType sym sec,
      asteriusStatics = ass
    }

marshalCmmLocalReg :: GHC.LocalReg -> CodeGen (UnresolvedLocalReg, ValueType)
marshalCmmLocalReg (GHC.LocalReg u t) = do
  vt <- marshalCmmType t
  pure (UniqueLocalReg (GHC.getKey u) vt, vt)

marshalTypedCmmLocalReg ::
  GHC.LocalReg -> ValueType -> CodeGen UnresolvedLocalReg
marshalTypedCmmLocalReg r vt = do
  (lr, vt') <- marshalCmmLocalReg r
  if vt == vt' then pure lr else throwM $ UnsupportedCmmExpr $ showBS r

marshalCmmGlobalReg :: GHC.GlobalReg -> CodeGen UnresolvedGlobalReg
marshalCmmGlobalReg r = case r of
  GHC.VanillaReg i _ -> pure $ VanillaReg i
  GHC.FloatReg i -> pure $ FloatReg i
  GHC.DoubleReg i -> pure $ DoubleReg i
  GHC.LongReg i -> pure $ LongReg i
  GHC.Sp -> pure Sp
  GHC.SpLim -> pure SpLim
  GHC.Hp -> pure Hp
  GHC.HpLim -> pure HpLim
  GHC.CCCS -> pure CCCS
  GHC.CurrentTSO -> pure CurrentTSO
  GHC.CurrentNursery -> pure CurrentNursery
  GHC.HpAlloc -> pure HpAlloc
  GHC.EagerBlackholeInfo -> pure EagerBlackholeInfo
  GHC.GCEnter1 -> pure GCEnter1
  GHC.GCFun -> pure GCFun
  GHC.BaseReg -> pure BaseReg
  _ -> throwM $ UnsupportedCmmGlobalReg $ showBS r

marshalCmmLit :: GHC.CmmLit -> CodeGen (Expression, ValueType)
marshalCmmLit lit = case lit of
  GHC.CmmInt x w ->
    dispatchCmmWidth
      w
      (ConstI32 $ fromIntegral x, I32)
      (ConstI64 $ fromIntegral x, I64)
  GHC.CmmFloat x w ->
    dispatchCmmWidth
      w
      (ConstF32 $ fromRational x, F32)
      (ConstF64 $ fromRational x, F64)
  GHC.CmmLabel clbl -> do
    sym <- marshalCLabel clbl
    pure (Symbol {unresolvedSymbol = sym, symbolOffset = 0}, I32)
  GHC.CmmLabelOff clbl o -> do
    sym <- marshalCLabel clbl
    pure (Symbol {unresolvedSymbol = sym, symbolOffset = o}, I32)
  _ -> throwM $ UnsupportedCmmLit $ showBS lit

marshalCmmLoad :: GHC.CmmExpr -> GHC.CmmType -> CodeGen (Expression, ValueType)
marshalCmmLoad p t = do
  pv <- marshalAndCheckCmmExpr p I32
  join $
    dispatchAllCmmWidth
      (GHC.typeWidth t)
      ( pure
          ( Load
              { signed = False,
                bytes = 1,
                offset = 0,
                valueType = I32,
                ptr = pv
              },
            I32
          )
      )
      ( pure
          ( Load
              { signed = False,
                bytes = 2,
                offset = 0,
                valueType = I32,
                ptr = pv
              },
            I32
          )
      )
      ( do
          vt <- marshalCmmType t
          pure
            ( Load
                { signed = False,
                  bytes = 4,
                  offset = 0,
                  valueType = vt,
                  ptr = pv
                },
              vt
            )
      )
      ( do
          vt <- marshalCmmType t
          pure
            ( Load
                { signed = False,
                  bytes = 8,
                  offset = 0,
                  valueType = vt,
                  ptr = pv
                },
              vt
            )
      )

marshalCmmReg :: GHC.CmmReg -> CodeGen (Expression, ValueType)
marshalCmmReg r = case r of
  GHC.CmmLocal lr -> do
    (lr_k, lr_vt) <- marshalCmmLocalReg lr
    pure (UnresolvedGetLocal {unresolvedLocalReg = lr_k}, lr_vt)
  GHC.CmmGlobal gr -> do
    gr_k <- marshalCmmGlobalReg gr
    pure (unresolvedGetGlobal gr_k, unresolvedGlobalRegType gr_k)

marshalCmmRegOff :: GHC.CmmReg -> Int -> CodeGen (Expression, ValueType)
marshalCmmRegOff r o = do
  (re, vt) <- marshalCmmReg r
  case vt of
    I32 ->
      pure
        ( Binary
            { binaryOp = AddInt32,
              operand0 = re,
              operand1 = ConstI32 $ fromIntegral o
            },
          vt
        )
    I64 ->
      pure
        ( Binary
            { binaryOp = AddInt64,
              operand0 = re,
              operand1 = ConstI64 $ fromIntegral o
            },
          vt
        )
    _ -> throwM $ UnsupportedCmmExpr $ showBS $ GHC.CmmRegOff r o

marshalCmmBinMachOp ::
  BinaryOp ->
  ValueType ->
  ValueType ->
  ValueType ->
  BinaryOp ->
  ValueType ->
  ValueType ->
  ValueType ->
  GHC.Width ->
  GHC.CmmExpr ->
  GHC.CmmExpr ->
  CodeGen (Expression, ValueType)
marshalCmmBinMachOp o32 tx32 ty32 tr32 o64 tx64 ty64 tr64 w x y =
  join $
    dispatchCmmWidth
      w
      ( do
          xe <- marshalAndCheckCmmExpr x tx32
          ye <- marshalAndCheckCmmExpr y ty32
          pure (Binary {binaryOp = o32, operand0 = xe, operand1 = ye}, tr32)
      )
      ( do
          xe <- marshalAndCheckCmmExpr x tx64
          ye <- marshalAndCheckCmmExpr y ty64
          pure (Binary {binaryOp = o64, operand0 = xe, operand1 = ye}, tr64)
      )

-- Should this logic be pushed into `marshalAndCheckCmmExpr?
marshalCmmHomoConvMachOp ::
  UnaryOp ->
  UnaryOp ->
  ValueType ->
  ValueType ->
  GHC.Width ->
  GHC.Width ->
  ShouldSext ->
  GHC.CmmExpr ->
  CodeGen (Expression, ValueType)
marshalCmmHomoConvMachOp o36 o63 t32 t64 w0 w1 sext x
  | (w0 == GHC.W8 || w0 == GHC.W16) && (w1 == GHC.W32 || w1 == GHC.W64) =
    -- we are extending from {W8, W16} to {W32, W64}. Sign extension
    -- semantics matters here.
    do
      (xe, _) <- marshalCmmExpr x
      pure
        ( genExtend
            (if w0 == GHC.W8 then 1 else 2)
            (if w1 == GHC.W32 then I32 else I64)
            sext
            xe,
          if w1 == GHC.W64 then I64 else I32
        )
  | (w0 == GHC.W32 || w0 == GHC.W64) && (w1 == GHC.W8 || w1 == GHC.W16) =
    -- we are wrapping from {32, 64} to {8, 16}
    do
      (xe, _) <- marshalCmmExpr x
      pure
        ( genWrap (if w0 == GHC.W32 then I32 else I64) (GHC.widthInBytes w1) xe,
          I32
        )
  | otherwise =
    -- we are converting from {32, 64} to {32, 64} of floating point / int
    do
      (o, t, tr) <- dispatchCmmWidth w1 (o63, t64, t32) (o36, t32, t64)
      xe <- marshalAndCheckCmmExpr x t
      pure (Unary {unaryOp = o, operand0 = xe}, tr)

marshalCmmHeteroConvMachOp ::
  UnaryOp ->
  UnaryOp ->
  UnaryOp ->
  UnaryOp ->
  ValueType ->
  ValueType ->
  ValueType ->
  ValueType ->
  GHC.Width ->
  GHC.Width ->
  GHC.CmmExpr ->
  CodeGen (Expression, ValueType)
marshalCmmHeteroConvMachOp o33 o36 o63 o66 tx32 ty32 tx64 ty64 w0 w1 x = do
  (g0, g1) <-
    dispatchCmmWidth
      w0
      ((o33, tx32, ty32), (o36, tx32, ty64))
      ((o63, tx64, ty32), (o66, tx64, ty64))
  (o, t, tr) <- dispatchCmmWidth w1 g0 g1
  xe <- marshalAndCheckCmmExpr x t
  pure (Unary {unaryOp = o, operand0 = xe}, tr)

marshalCmmMachOp ::
  GHC.MachOp -> [GHC.CmmExpr] -> CodeGen (Expression, ValueType)
-- Integer operations (insensitive to signed/unsigned)
marshalCmmMachOp (GHC.MO_Add w) [x, y] =
  marshalCmmBinMachOp AddInt32 I32 I32 I32 AddInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_Sub w) [x, y] =
  marshalCmmBinMachOp SubInt32 I32 I32 I32 SubInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_Eq w) [x, y] =
  marshalCmmBinMachOp EqInt32 I32 I32 I32 EqInt64 I64 I64 I32 w x y
marshalCmmMachOp (GHC.MO_Ne w) [x, y] =
  marshalCmmBinMachOp NeInt32 I32 I32 I32 NeInt64 I64 I64 I32 w x y
marshalCmmMachOp (GHC.MO_Mul w) [x, y] =
  marshalCmmBinMachOp MulInt32 I32 I32 I32 MulInt64 I64 I64 I64 w x y
-- Signed multiply/divide
marshalCmmMachOp (GHC.MO_S_Quot w) [x, y] =
  marshalCmmBinMachOp DivSInt32 I32 I32 I32 DivSInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_S_Rem w) [x, y] =
  marshalCmmBinMachOp RemSInt32 I32 I32 I32 RemSInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_S_Neg w) [x] =
  join $
    dispatchCmmWidth
      w
      ( do
          xe <- marshalAndCheckCmmExpr x I32
          pure
            ( Binary {binaryOp = SubInt32, operand0 = ConstI32 0, operand1 = xe},
              I32
            )
      )
      ( do
          xe <- marshalAndCheckCmmExpr x I64
          pure
            ( Binary {binaryOp = SubInt64, operand0 = ConstI64 0, operand1 = xe},
              I64
            )
      )
-- Unsigned multiply/divide
marshalCmmMachOp (GHC.MO_U_Quot w) [x, y] =
  marshalCmmBinMachOp DivUInt32 I32 I32 I32 DivUInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_U_Rem w) [x, y] =
  marshalCmmBinMachOp RemUInt32 I32 I32 I32 RemUInt64 I64 I64 I64 w x y
-- Signed comparisons
marshalCmmMachOp (GHC.MO_S_Ge w) [x, y] =
  marshalCmmBinMachOp GeSInt32 I32 I32 I32 GeSInt64 I64 I64 I32 w x y
marshalCmmMachOp (GHC.MO_S_Le w) [x, y] =
  marshalCmmBinMachOp LeSInt32 I32 I32 I32 LeSInt64 I64 I64 I32 w x y
marshalCmmMachOp (GHC.MO_S_Gt w) [x, y] =
  marshalCmmBinMachOp GtSInt32 I32 I32 I32 GtSInt64 I64 I64 I32 w x y
marshalCmmMachOp (GHC.MO_S_Lt w) [x, y] =
  marshalCmmBinMachOp LtSInt32 I32 I32 I32 LtSInt64 I64 I64 I32 w x y
-- Unsigned comparisons
marshalCmmMachOp (GHC.MO_U_Ge w) [x, y] =
  marshalCmmBinMachOp GeUInt32 I32 I32 I32 GeUInt64 I64 I64 I32 w x y
marshalCmmMachOp (GHC.MO_U_Le w) [x, y] =
  marshalCmmBinMachOp LeUInt32 I32 I32 I32 LeUInt64 I64 I64 I32 w x y
marshalCmmMachOp (GHC.MO_U_Gt w) [x, y] =
  marshalCmmBinMachOp GtUInt32 I32 I32 I32 GtUInt64 I64 I64 I32 w x y
marshalCmmMachOp (GHC.MO_U_Lt w) [x, y] =
  marshalCmmBinMachOp LtUInt32 I32 I32 I32 LtUInt64 I64 I64 I32 w x y
-- Floating point arithmetic
marshalCmmMachOp (GHC.MO_F_Add w) [x, y] =
  marshalCmmBinMachOp AddFloat32 F32 F32 F32 AddFloat64 F64 F64 F64 w x y
marshalCmmMachOp (GHC.MO_F_Sub w) [x, y] =
  marshalCmmBinMachOp SubFloat32 F32 F32 F32 SubFloat64 F64 F64 F64 w x y
marshalCmmMachOp (GHC.MO_F_Neg w) [x] =
  join $
    dispatchCmmWidth
      w
      ( do
          xe <- marshalAndCheckCmmExpr x F32
          pure (Unary {unaryOp = NegFloat32, operand0 = xe}, F32)
      )
      ( do
          xe <- marshalAndCheckCmmExpr x F64
          pure (Unary {unaryOp = NegFloat64, operand0 = xe}, F64)
      )
marshalCmmMachOp (GHC.MO_F_Mul w) [x, y] =
  marshalCmmBinMachOp MulFloat32 F32 F32 F32 MulFloat64 F64 F64 F64 w x y
marshalCmmMachOp (GHC.MO_F_Quot w) [x, y] =
  marshalCmmBinMachOp DivFloat32 F32 F32 F32 DivFloat64 F64 F64 F64 w x y
  -- Floating point comparison
marshalCmmMachOp (GHC.MO_F_Eq w) [x, y] =
  marshalCmmBinMachOp EqFloat32 F32 F32 I32 EqFloat64 F64 F64 I32 w x y
marshalCmmMachOp (GHC.MO_F_Ne w) [x, y] =
  marshalCmmBinMachOp NeFloat32 F32 F32 I32 NeFloat64 F64 F64 I32 w x y
marshalCmmMachOp (GHC.MO_F_Ge w) [x, y] =
  marshalCmmBinMachOp GeFloat32 F32 F32 I32 GeFloat64 F64 F64 I32 w x y
marshalCmmMachOp (GHC.MO_F_Le w) [x, y] =
  marshalCmmBinMachOp LeFloat32 F32 F32 I32 LeFloat64 F64 F64 I32 w x y
marshalCmmMachOp (GHC.MO_F_Gt w) [x, y] =
  marshalCmmBinMachOp GtFloat32 F32 F32 I32 GtFloat64 F64 F64 I32 w x y
marshalCmmMachOp (GHC.MO_F_Lt w) [x, y] =
  marshalCmmBinMachOp LtFloat32 F32 F32 I32 LtFloat64 F64 F64 I32 w x y
-- Bitwise operations. Not all of these may be supported at all sizes,
-- and only integral Widths are valid.
marshalCmmMachOp (GHC.MO_And w) [x, y] =
  marshalCmmBinMachOp AndInt32 I32 I32 I32 AndInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_Or w) [x, y] =
  marshalCmmBinMachOp OrInt32 I32 I32 I32 OrInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_Xor w) [x, y] =
  marshalCmmBinMachOp XorInt32 I32 I32 I32 XorInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_Not w) [x] =
  join $
    dispatchCmmWidth
      w
      ( do
          xe <- marshalAndCheckCmmExpr x I32
          pure
            ( Binary
                { binaryOp = XorInt32,
                  operand0 = xe,
                  operand1 = ConstI32 0xFFFFFFFF
                },
              I32
            )
      )
      ( do
          xe <- marshalAndCheckCmmExpr x I64
          pure
            ( Binary
                { binaryOp = XorInt64,
                  operand0 = xe,
                  operand1 = ConstI64 0xFFFFFFFFFFFFFFFF
                },
              I64
            )
      )
marshalCmmMachOp (GHC.MO_Shl w) [x, y] =
  marshalCmmBinMachOp ShlInt32 I32 I32 I32 ShlInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_U_Shr w) [x, y] =
  marshalCmmBinMachOp ShrUInt32 I32 I32 I32 ShrUInt64 I64 I64 I64 w x y
marshalCmmMachOp (GHC.MO_S_Shr w) [x, y] =
  marshalCmmBinMachOp ShrSInt32 I32 I32 I32 ShrSInt64 I64 I64 I64 w x y
-- Conversions. Some of these will be NOPs.
-- Floating-point conversions use the signed variant.
marshalCmmMachOp (GHC.MO_SF_Conv w0 w1) [x] =
  marshalCmmHeteroConvMachOp
    ConvertSInt32ToFloat32
    ConvertSInt32ToFloat64
    ConvertSInt64ToFloat32
    ConvertSInt64ToFloat64
    I32
    F32
    I64
    F64
    w0
    w1
    x
marshalCmmMachOp (GHC.MO_FS_Conv w0 w1) [x] =
  marshalCmmHeteroConvMachOp
    TruncSFloat32ToInt32
    TruncSFloat32ToInt64
    TruncSFloat64ToInt32
    TruncSFloat64ToInt64
    F32
    I32
    F64
    I64
    w0
    w1
    x
--marshalCmmMachOp (GHC.MO_SS_Conv w0 w1) [x] =
--  marshalCmmHomoConvMachOp ExtendSInt32 WrapInt64 I32 I64 w0 w1 Sext x
--marshalCmmMachOp (GHC.MO_UU_Conv w0 w1) [x] =
--  marshalCmmHomoConvMachOp ExtendUInt32 WrapInt64 I32 I64 w0 w1 NoSext x
--marshalCmmMachOp (GHC.MO_FF_Conv w0 w1) [x] =
--  marshalCmmHomoConvMachOp PromoteFloat32 DemoteFloat64 F32 F64 w0 w1 Sext x
-- Unhandled cases
--   -- Signed multiply/divide
--   MO_S_MulMayOflo Width       -- nonzero if signed multiply overflows
--   -- Unsigned multiply/divide
--   MO_U_MulMayOflo Width       -- nonzero if unsigned multiply overflows
--   -- Vector element insertion and extraction operations
--   MO_V_Insert  Length Width   -- Insert scalar into vector
--   MO_V_Extract Length Width   -- Extract scalar from vector
--   -- Integer vector operations
--   MO_V_Add Length Width
--   MO_V_Sub Length Width
--   MO_V_Mul Length Width
--   -- Signed vector multiply/divide
--   MO_VS_Quot Length Width
--   MO_VS_Rem  Length Width
--   MO_VS_Neg  Length Width
--   -- Unsigned vector multiply/divide
--   MO_VU_Quot Length Width
--   MO_VU_Rem  Length Width
--   -- Floting point vector element insertion and extraction operations
--   MO_VF_Insert  Length Width   -- Insert scalar into vector
--   MO_VF_Extract Length Width   -- Extract scalar from vector
--   -- Floating point vector operations
--   MO_VF_Add  Length Width
--   MO_VF_Sub  Length Width
--   MO_VF_Neg  Length Width      -- unary negation
--   MO_VF_Mul  Length Width
--   MO_VF_Quot Length Width
--   -- Alignment check (for -falignment-sanitisation)
--   MO_AlignmentCheck Int Width
marshalCmmMachOp op xs =
  throwM $ UnsupportedTodo

marshalCmmExpr :: GHC.CmmExpr -> CodeGen (Expression, ValueType)
marshalCmmExpr cmm_expr = case cmm_expr of
  GHC.CmmLit lit -> marshalCmmLit lit
  GHC.CmmLoad p t -> marshalCmmLoad p t
  GHC.CmmReg r -> marshalCmmReg r
  GHC.CmmMachOp op xs -> marshalCmmMachOp op xs
  GHC.CmmRegOff r o -> marshalCmmRegOff r o
  _ -> throwM $ UnsupportedCmmExpr $ showBS cmm_expr

marshalAndCheckCmmExpr :: GHC.CmmExpr -> ValueType -> CodeGen Expression
marshalAndCheckCmmExpr cmm_expr dest_vt = do
  (src_expr, src_vt) <- marshalCmmExpr cmm_expr
  if src_vt == dest_vt then pure src_expr
    else throwM $
        UnsupportedImplicitCasting src_expr src_vt dest_vt

marshalCmmUnPrimCall ::
  ValueType -> -- result type
  GHC.LocalReg -> -- result destination
  ValueType -> -- operand type
  GHC.CmmExpr -> -- operand
  (Expression -> Expression) -> -- operation
  CodeGen [Expression]
marshalCmmUnPrimCall retTyp ret vTyp v op = do
  lr <- marshalTypedCmmLocalReg ret retTyp
  xe <- marshalAndCheckCmmExpr v vTyp
  pure [UnresolvedSetLocal {unresolvedLocalReg = lr, value = op xe}]

marshalCmmQuotRemPrimCall ::
  UnresolvedLocalReg ->
  UnresolvedLocalReg ->
  BinaryOp ->
  BinaryOp ->
  ValueType ->
  GHC.LocalReg ->
  GHC.LocalReg ->
  GHC.CmmExpr ->
  GHC.CmmExpr ->
  CodeGen [Expression]
marshalCmmQuotRemPrimCall tmp0 tmp1 qop rop vt qr rr x y = do
  qlr <- marshalTypedCmmLocalReg qr vt
  rlr <- marshalTypedCmmLocalReg rr vt
  xe <- marshalAndCheckCmmExpr x vt
  ye <- marshalAndCheckCmmExpr y vt
  pure
    [ UnresolvedSetLocal {unresolvedLocalReg = tmp0, value = xe},
      UnresolvedSetLocal {unresolvedLocalReg = tmp1, value = ye},
      UnresolvedSetLocal
        { unresolvedLocalReg = qlr,
          value = Binary
            { binaryOp = qop,
              operand0 = UnresolvedGetLocal {unresolvedLocalReg = tmp0},
              operand1 = UnresolvedGetLocal {unresolvedLocalReg = tmp1}
            }
        },
      UnresolvedSetLocal
        { unresolvedLocalReg = rlr,
          value = Binary
            { binaryOp = rop,
              operand0 = UnresolvedGetLocal {unresolvedLocalReg = tmp0},
              operand1 = UnresolvedGetLocal {unresolvedLocalReg = tmp1}
            }
        }
    ]

marshalCmmUnMathPrimCall ::
  BS.ByteString ->
  ValueType ->
  GHC.LocalReg ->
  GHC.CmmExpr ->
  CodeGen [Expression]
marshalCmmUnMathPrimCall op vt r x = do
  lr <- marshalTypedCmmLocalReg r vt
  xe <- marshalAndCheckCmmExpr x vt
  pure
    [ UnresolvedSetLocal
        { unresolvedLocalReg = lr,
          value =
            Call
              { target =
                  mkEntitySymbol $
                    op
                      <> (if vt == F32 then "f" else ""),
                operands = [xe],
                callReturnTypes = [vt],
                callHint = Just ([NoHint], [NoHint])
              }
        }
    ]

marshalCmmBinMathPrimCall ::
  BS.ByteString ->
  ValueType ->
  GHC.LocalReg ->
  GHC.CmmExpr ->
  GHC.CmmExpr ->
  CodeGen [Expression]
marshalCmmBinMathPrimCall op vt r x y = do
  lr <- marshalTypedCmmLocalReg r vt
  xe <- marshalAndCheckCmmExpr x vt
  ye <- marshalAndCheckCmmExpr y vt
  pure
    [ UnresolvedSetLocal
        { unresolvedLocalReg = lr,
          value =
            Call
              { target =
                  mkEntitySymbol $
                    op
                      <> (if vt == F32 then "f" else ""),
                operands = [xe, ye],
                callReturnTypes = [vt],
                callHint = Just ([NoHint, NoHint], [NoHint])
              }
        }
    ]

-- We follow the order of definition from:
-- https://github.com/ghc/ghc/blob/master/compiler/cmm/CmmMachOp.hs
marshalCmmPrimCall ::
  GHC.CallishMachOp ->
  [GHC.LocalReg] ->
  [GHC.CmmExpr] ->
  CodeGen [Expression]
marshalCmmPrimCall GHC.MO_F64_Pwr [r] [x, y] =
  marshalCmmBinMathPrimCall "pow" F64 r x y
marshalCmmPrimCall GHC.MO_F64_Sin [r] [x] =
  marshalCmmUnMathPrimCall "sin" F64 r x
marshalCmmPrimCall GHC.MO_F64_Cos [r] [x] =
  marshalCmmUnMathPrimCall "cos" F64 r x
marshalCmmPrimCall GHC.MO_F64_Tan [r] [x] =
  marshalCmmUnMathPrimCall "tan" F64 r x
marshalCmmPrimCall GHC.MO_F64_Sinh [r] [x] =
  marshalCmmUnMathPrimCall "sinh" F64 r x
marshalCmmPrimCall GHC.MO_F64_Cosh [r] [x] =
  marshalCmmUnMathPrimCall "cosh" F64 r x
marshalCmmPrimCall GHC.MO_F64_Tanh [r] [x] =
  marshalCmmUnMathPrimCall "tanh" F64 r x
marshalCmmPrimCall GHC.MO_F64_Asin [r] [x] =
  marshalCmmUnMathPrimCall "asin" F64 r x
marshalCmmPrimCall GHC.MO_F64_Acos [r] [x] =
  marshalCmmUnMathPrimCall "acos" F64 r x
marshalCmmPrimCall GHC.MO_F64_Atan [r] [x] =
  marshalCmmUnMathPrimCall "atan" F64 r x
marshalCmmPrimCall GHC.MO_F64_Asinh [r] [x] =
  marshalCmmUnMathPrimCall "asinh" F64 r x
marshalCmmPrimCall GHC.MO_F64_Acosh [r] [x] =
  marshalCmmUnMathPrimCall "acosh" F64 r x
marshalCmmPrimCall GHC.MO_F64_Atanh [r] [x] =
  marshalCmmUnMathPrimCall "atanh" F64 r x
marshalCmmPrimCall GHC.MO_F64_Log [r] [x] =
  marshalCmmUnMathPrimCall "log" F64 r x
marshalCmmPrimCall GHC.MO_F64_Exp [r] [x] =
  marshalCmmUnMathPrimCall "exp" F64 r x
marshalCmmPrimCall GHC.MO_F64_Fabs [r] [x] =
  marshalCmmUnPrimCall F64 r F64 x (Unary AbsFloat64)
marshalCmmPrimCall GHC.MO_F64_Sqrt [r] [x] =
  marshalCmmUnPrimCall F64 r F64 x (Unary SqrtFloat64)
-- 32 bit
marshalCmmPrimCall GHC.MO_F32_Pwr [r] [x, y] =
  marshalCmmBinMathPrimCall "pow" F32 r x y
marshalCmmPrimCall GHC.MO_F32_Sin [r] [x] =
  marshalCmmUnMathPrimCall "sin" F32 r x
marshalCmmPrimCall GHC.MO_F32_Cos [r] [x] =
  marshalCmmUnMathPrimCall "cos" F32 r x
marshalCmmPrimCall GHC.MO_F32_Tan [r] [x] =
  marshalCmmUnMathPrimCall "tan" F32 r x
marshalCmmPrimCall GHC.MO_F32_Sinh [r] [x] =
  marshalCmmUnMathPrimCall "sinh" F32 r x
marshalCmmPrimCall GHC.MO_F32_Cosh [r] [x] =
  marshalCmmUnMathPrimCall "cosh" F32 r x
marshalCmmPrimCall GHC.MO_F32_Tanh [r] [x] =
  marshalCmmUnMathPrimCall "tanh" F32 r x
marshalCmmPrimCall GHC.MO_F32_Asin [r] [x] =
  marshalCmmUnMathPrimCall "asin" F32 r x
marshalCmmPrimCall GHC.MO_F32_Acos [r] [x] =
  marshalCmmUnMathPrimCall "acos" F32 r x
marshalCmmPrimCall GHC.MO_F32_Atan [r] [x] =
  marshalCmmUnMathPrimCall "atan" F32 r x
marshalCmmPrimCall GHC.MO_F32_Asinh [r] [x] =
  marshalCmmUnMathPrimCall "asinh" F32 r x
marshalCmmPrimCall GHC.MO_F32_Acosh [r] [x] =
  marshalCmmUnMathPrimCall "acosh" F32 r x
marshalCmmPrimCall GHC.MO_F32_Atanh [r] [x] =
  marshalCmmUnMathPrimCall "atanh" F32 r x
marshalCmmPrimCall GHC.MO_F32_Log [r] [x] =
  marshalCmmUnMathPrimCall "log" F32 r x
marshalCmmPrimCall GHC.MO_F32_Exp [r] [x] =
  marshalCmmUnMathPrimCall "exp" F32 r x
marshalCmmPrimCall GHC.MO_F32_Fabs [r] [x] =
  marshalCmmUnPrimCall F32 r F32 x (Unary AbsFloat32)
marshalCmmPrimCall GHC.MO_F32_Sqrt [r] [x] =
  marshalCmmUnPrimCall F32 r F32 x (Unary SqrtFloat32)
marshalCmmPrimCall (GHC.MO_UF_Conv w) [r] [x] = do
  (op, ft) <-
    dispatchCmmWidth
      w
      (ConvertUInt64ToFloat32, F32)
      (ConvertUInt64ToFloat64, F64)
  lr <- marshalTypedCmmLocalReg r ft
  xe <- marshalAndCheckCmmExpr x I32
  pure
    [ UnresolvedSetLocal
        { unresolvedLocalReg = lr,
          value = Unary {unaryOp = op, operand0 = xe}
        }
    ]
marshalCmmPrimCall (GHC.MO_S_QuotRem w) [qr, rr] [x, y] =
  join $
    dispatchCmmWidth
      w
      ( marshalCmmQuotRemPrimCall
          QuotRemI32X
          QuotRemI32Y
          DivSInt32
          RemSInt32
          I32
          qr
          rr
          x
          y
      )
      ( marshalCmmQuotRemPrimCall
          QuotRemI64X
          QuotRemI64Y
          DivSInt64
          RemSInt64
          I64
          qr
          rr
          x
          y
      )
marshalCmmPrimCall (GHC.MO_U_QuotRem w) [qr, rr] [x, y] =
  join $
    dispatchCmmWidth
      w
      ( marshalCmmQuotRemPrimCall
          QuotRemI32X
          QuotRemI32Y
          DivUInt32
          RemUInt32
          I32
          qr
          rr
          x
          y
      )
      ( marshalCmmQuotRemPrimCall
          QuotRemI64X
          QuotRemI64Y
          DivUInt64
          RemUInt64
          I64
          qr
          rr
          x
          y
      )
marshalCmmPrimCall GHC.MO_WriteBarrier _ _ = pure []
marshalCmmPrimCall GHC.MO_Touch _ _ = pure []
marshalCmmPrimCall (GHC.MO_Prefetch_Data _) _ _ = pure []
marshalCmmPrimCall (GHC.MO_Memcpy _) [] [_dst, _src, _n] = do
  dst <- marshalAndCheckCmmExpr _dst I32
  src <- marshalAndCheckCmmExpr _src I32
  n <- marshalAndCheckCmmExpr _n I32
  pure [memcpy dst src n]
marshalCmmPrimCall (GHC.MO_Memset _) [] [_dst, _c, _n] = do
  dst <- marshalAndCheckCmmExpr _dst I32
  c <- marshalAndCheckCmmExpr _c I32
  n <- marshalAndCheckCmmExpr _n I32
  pure [memset dst c n]
marshalCmmPrimCall (GHC.MO_Memmove _) [] [_dst, _src, _n] = do
  dst <- marshalAndCheckCmmExpr _dst I32
  src <- marshalAndCheckCmmExpr _src I32
  n <- marshalAndCheckCmmExpr _n I32
  pure [memmove dst src n]
marshalCmmPrimCall (GHC.MO_Memcmp _) [_cres] [_ptr1, _ptr2, _n] = do
  cres <- marshalTypedCmmLocalReg _cres I32
  ptr1 <- marshalAndCheckCmmExpr _ptr1 I32
  ptr2 <- marshalAndCheckCmmExpr _ptr2 I32
  n <- marshalAndCheckCmmExpr _n I32
  pure
    [ UnresolvedSetLocal
        { unresolvedLocalReg = cres,
          value = memcmp ptr1 ptr2 n
        }
    ]
marshalCmmPrimCall (GHC.MO_PopCnt GHC.W64) [r] [x] =
  marshalCmmUnPrimCall I32 r I64 x (wrapInt64. popcntInt64)
marshalCmmPrimCall (GHC.MO_PopCnt GHC.W32) [r] [x] = do
  marshalCmmUnPrimCall I32 r I32 x popcntInt32
marshalCmmPrimCall (GHC.MO_PopCnt GHC.W16) [r] [x] = do
  marshalCmmUnPrimCall
    I32
    r
    I32
    x
    (popcntInt32 . andInt32 (constI32 0xFFFF))
marshalCmmPrimCall (GHC.MO_PopCnt GHC.W8) [r] [x] = do
  marshalCmmUnPrimCall
    I32
    r
    I32
    x
    (popcntInt32 . andInt32 (constI32 0xFF))
-- Unhandled: MO_Pdep Width
-- Unhandled: MO_Pext Width
marshalCmmPrimCall (GHC.MO_Clz GHC.W64) [r] [x] =
  marshalCmmUnPrimCall I32 r I64 x (wrapInt64. clzInt64)
marshalCmmPrimCall (GHC.MO_Clz GHC.W32) [r] [x] =
  marshalCmmUnPrimCall I32 r I32 x clzInt32
marshalCmmPrimCall (GHC.MO_Clz GHC.W16) [r] [x] =
  marshalCmmUnPrimCall
    I32
    r
    I32
    x
    ( clzInt32
        . orInt32 (constI32 0x8000)
        . (`shlInt32` constI32 16)
    )
marshalCmmPrimCall (GHC.MO_Clz GHC.W8) [r] [x] =
  marshalCmmUnPrimCall
    I32
    r
    I32
    x
    ( clzInt32
        . orInt32 (constI32 0x800000)
        . (`shlInt32` constI32 24)
    )
marshalCmmPrimCall (GHC.MO_Ctz GHC.W64) [r] [x] =
  marshalCmmUnPrimCall I32 r I64 x (wrapInt64 . ctzInt64)
marshalCmmPrimCall (GHC.MO_Ctz GHC.W32) [r] [x] =
  marshalCmmUnPrimCall I32 r I32 x ctzInt32
marshalCmmPrimCall (GHC.MO_Ctz GHC.W16) [r] [x] =
  marshalCmmUnPrimCall
    I32
    r
    I32
    x
    (ctzInt32 . orInt32 (constI32 0x10000))
marshalCmmPrimCall (GHC.MO_Ctz GHC.W8) [r] [x] =
  marshalCmmUnPrimCall
    I32
    r
    I32
    x
    (ctzInt32 . orInt32 (constI32 0x100))
-- r = result, o = overflow
-- see also: GHC.Prim.subWordC#
marshalCmmPrimCall (GHC.MO_SubWordC GHC.W64) [r, o] [x, y] = do
  (xr, _) <- marshalCmmExpr x
  (yr, _) <- marshalCmmExpr y
  lr <- marshalTypedCmmLocalReg r I64
  lo <- marshalTypedCmmLocalReg o I64
  let x_minus_y = Binary {binaryOp = SubInt64, operand0 = xr, operand1 = yr}
  let overflow = Binary {binaryOp = LtUInt64, operand0 = xr, operand1 = yr}
  let overflow_sext = Unary {unaryOp = ExtendUInt32, operand0 = overflow}
  pure
    [ UnresolvedSetLocal {unresolvedLocalReg = lr, value = x_minus_y},
      UnresolvedSetLocal {unresolvedLocalReg = lo, value = overflow_sext}
    ]
-- r = result, o = overflow
-- see also: GHC.Prim.addWordC#
marshalCmmPrimCall (GHC.MO_AddWordC GHC.W64) [r, o] [x, y] = do
  (xr, _) <- marshalCmmExpr x
  (yr, _) <- marshalCmmExpr y
  lr <- marshalTypedCmmLocalReg r I64
  -- x + y > maxbound
  -- y + x > maxbound
  -- y > maxbound - x
  lo <- marshalTypedCmmLocalReg o I64
  let x_plus_y = Binary {binaryOp = AddInt64, operand0 = xr, operand1 = yr}
  let maxbound_minus_x = Binary
        { binaryOp = SubInt64,
          operand0 = ConstI64 0xFFFFFFFFFFFFFFFF,
          operand1 = xr
        }
  let overflow = Binary
        { binaryOp = GtUInt64,
          operand0 = yr,
          operand1 = maxbound_minus_x
        }
  let overflow_sext = Unary {unaryOp = ExtendUInt32, operand0 = overflow}
  pure
    [ UnresolvedSetLocal {unresolvedLocalReg = lr, value = x_plus_y},
      UnresolvedSetLocal {unresolvedLocalReg = lo, value = overflow_sext}
    ]
-- See also: GHC.Prim.plusWord2
-- add unsigned: return (carry, result)
marshalCmmPrimCall (GHC.MO_Add2 GHC.W64) [o, r] [x, y] = do
  (xr, _) <- marshalCmmExpr x
  (yr, _) <- marshalCmmExpr y
  lr <- marshalTypedCmmLocalReg r I64
  -- y + x > maxbound
  -- y > maxbound - x
  lo <- marshalTypedCmmLocalReg o I64
  let x_plus_y = Binary {binaryOp = AddInt64, operand0 = xr, operand1 = yr}
  let x_minus_maxbound = Binary
        { binaryOp = SubInt64,
          operand0 = ConstI64 0xFFFFFFFFFFFFFFFF,
          operand1 = xr
        }
  let overflow = Binary
        { binaryOp = GtUInt64,
          operand0 = yr,
          operand1 = x_minus_maxbound
        }
  let overflow_sext = Unary {unaryOp = ExtendUInt32, operand0 = overflow}
  pure
    [ UnresolvedSetLocal {unresolvedLocalReg = lr, value = x_plus_y},
      UnresolvedSetLocal {unresolvedLocalReg = lo, value = overflow_sext}
    ]
-- See also: GHC.Prim.addIntC#
-- add signed: return (result, overflow)
marshalCmmPrimCall (GHC.MO_AddIntC GHC.W64) [r, o] [x, y] = do
  (xr, _) <- marshalCmmExpr x
  (yr, _) <- marshalCmmExpr y
  -- Copied from ghc-testsuite/numeric/CarryOverflow.hs:
  -- where ltTest x y =
  --         let r = x + y in (y > 0 && r < x) || (y < 0 && r > x)
  lr <- marshalTypedCmmLocalReg r I64
  -- y + x > maxbound
  -- y > maxbound - x
  lo <- marshalTypedCmmLocalReg o I64
  let x_plus_y = Binary {binaryOp = AddInt64, operand0 = xr, operand1 = yr}
  let y_positive_test =
        Binary {binaryOp = GtSInt64, operand0 = yr, operand1 = (ConstI64 0)}
  let y_negative_test =
        Binary {binaryOp = LtSInt64, operand0 = yr, operand1 = (ConstI64 0)}
  let x_plus_y_gt_x = Binary GtSInt64 x_plus_y xr
  let x_plus_y_lt_x = Binary LtSInt64 x_plus_y xr
  let clause_1 = Binary MulInt32 y_positive_test x_plus_y_lt_x
  let clause_2 = Binary MulInt32 y_negative_test x_plus_y_gt_x
  -- Addition is OK to express OR since only of the clauses can be
  -- true as they are mutually exclusive.
  let overflow = Binary AddInt32 clause_1 clause_2
  let overflow_sext = Unary {unaryOp = ExtendUInt32, operand0 = overflow}
  pure
    [ UnresolvedSetLocal {unresolvedLocalReg = lr, value = x_plus_y},
      UnresolvedSetLocal {unresolvedLocalReg = lo, value = overflow_sext}
    ]
-- subtract signed, reporting overflow:
-- see also: GHC.Prim.SubIntC#
-- returns (result, overflow)
marshalCmmPrimCall (GHC.MO_SubIntC GHC.W64) [r, o] [x, y] = do
  (xr, _) <- marshalCmmExpr x
  (yr, _) <- marshalCmmExpr y
  lr <- marshalTypedCmmLocalReg r I64
  lo <- marshalTypedCmmLocalReg o I64
  -- Copied from ghc-testsuite/numeric/CarryOverflow.hs:
  --  where ltTest x y =
  --        let r = x - y in (y > 0 && r > x) || (y < 0 && r < x)
  let x_minus_y = Binary {binaryOp = SubInt64, operand0 = xr, operand1 = yr}
  let y_positive_test =
        Binary {binaryOp = GtSInt64, operand0 = yr, operand1 = (ConstI64 0)}
  let y_negative_test =
        Binary {binaryOp = LtSInt64, operand0 = yr, operand1 = (ConstI64 0)}
  let x_minus_y_gt_x = Binary GtSInt64 x_minus_y xr
  let x_minus_y_lt_x = Binary LtSInt64 x_minus_y xr
  let clause_1 = Binary MulInt32 y_positive_test x_minus_y_gt_x
  let clause_2 = Binary MulInt32 y_negative_test x_minus_y_lt_x
  -- Addition is OK to express OR since only of the clauses can be
  -- true as they are mutually exclusive.
  let overflow = Binary AddInt32 clause_1 clause_2
  let overflow_sext = Unary {unaryOp = ExtendUInt32, operand0 = overflow}
  pure
    [ UnresolvedSetLocal {unresolvedLocalReg = lr, value = x_minus_y},
      UnresolvedSetLocal {unresolvedLocalReg = lo, value = overflow_sext}
    ]
-- high = high bits of result, low = low bits of result.
-- see also: GHC.Prim.timesWord2#
marshalCmmPrimCall (GHC.MO_U_Mul2 GHC.W64) [hi, lo] [x, y] = do
  (xr, _) <- marshalCmmExpr x
  (yr, _) <- marshalCmmExpr y
  hir <- marshalTypedCmmLocalReg hi I64
  lor <- marshalTypedCmmLocalReg lo I64
  -- Smash the high and low 32 bits together to create a 64 bit
  -- number.
  let smash32IntTo64 hi32 lo32 =
        Binary
          OrInt64
          (Binary ShlInt64 (Unary ExtendUInt32 hi32) (ConstI64 32))
          (Unary ExtendUInt32 lo32)
  -- mask the `n32`th block of v, counting blocks from the lowest bit.
  let mask32 v n32 =
        Unary WrapInt64 $
          Binary
            AndInt64
            (Binary ShrUInt64 v (ConstI64 (n32 * 32)))
            (ConstI64 0xFFFFFFFF)
  let hiout = UnresolvedSetLocal
        { unresolvedLocalReg = hir,
          value = smash32IntTo64
            CallImport
              { target' = "__asterius_mul2",
                operands =
                  [ mask32 xr 1,
                    mask32 xr 0,
                    mask32 yr 1,
                    mask32 yr 0,
                    ConstI32 3
                  ],
                callImportReturnTypes = [I32]
              }
            CallImport
              { target' = "__asterius_mul2",
                operands =
                  [ mask32 xr 1,
                    mask32 xr 0,
                    mask32 yr 1,
                    mask32 yr 0,
                    ConstI32 2
                  ],
                callImportReturnTypes = [I32]
              }
        }
  let loout = UnresolvedSetLocal
        { unresolvedLocalReg = lor,
          value = smash32IntTo64
            CallImport
              { target' = "__asterius_mul2",
                operands =
                  [ mask32 xr 1,
                    mask32 xr 0,
                    mask32 yr 1,
                    mask32 yr 0,
                    ConstI32 1
                  ],
                callImportReturnTypes = [I32]
              }
            CallImport
              { target' = "__asterius_mul2",
                operands =
                  [ mask32 xr 1,
                    mask32 xr 0,
                    mask32 yr 1,
                    mask32 yr 0,
                    ConstI32 0
                  ],
                callImportReturnTypes = [I32]
              }
        }
  pure [hiout, loout]
-- See also: QuotRemWord2#
marshalCmmPrimCall (GHC.MO_U_QuotRem2 GHC.W64) [q, r] [lhsHi, lhsLo, rhs] = do
  quotr <- marshalTypedCmmLocalReg q I64
  remr <- marshalTypedCmmLocalReg r I64
  (lhsHir, _) <- marshalCmmExpr lhsHi
  (lhsLor, _) <- marshalCmmExpr lhsLo
  (rhsr, _) <- marshalCmmExpr rhs
  -- Smash the high and low 32 bits together to create a 64 bit
  -- number.
  let smash32IntTo64 hi32 lo32 =
        Binary
          OrInt64
          (Binary ShlInt64 (Unary ExtendUInt32 hi32) (ConstI64 32))
          (Unary ExtendUInt32 lo32)
  -- mask the `n32`th block of v, counting blocks from the lowest bit.
  let mask32 v n32 =
        Unary WrapInt64 $
          Binary
            AndInt64
            (Binary ShrUInt64 v (ConstI64 (n32 * 32)))
            (ConstI64 0xFFFFFFFF)
  let quotout = UnresolvedSetLocal
        { unresolvedLocalReg = quotr,
          value = smash32IntTo64
            CallImport
              { target' = "__asterius_quotrem2_quotient",
                operands =
                  [ mask32 lhsHir 1,
                    mask32 lhsHir 0,
                    mask32 lhsLor 1,
                    mask32 lhsLor 0,
                    mask32 rhsr 1,
                    mask32 rhsr 0,
                    ConstI32 1
                  ],
                callImportReturnTypes = [I32]
              }
            CallImport
              { target' = "__asterius_quotrem2_quotient",
                operands =
                  [ mask32 lhsHir 1,
                    mask32 lhsHir 0,
                    mask32 lhsLor 1,
                    mask32 lhsLor 0,
                    mask32 rhsr 1,
                    mask32 rhsr 0,
                    ConstI32 0
                  ],
                callImportReturnTypes = [I32]
              }
        }
  let remout = UnresolvedSetLocal
        { unresolvedLocalReg = remr,
          value = smash32IntTo64
            CallImport
              { target' = "__asterius_quotrem2_remainder",
                operands =
                  [ mask32 lhsHir 1,
                    mask32 lhsHir 0,
                    mask32 lhsLor 1,
                    mask32 lhsLor 0,
                    mask32 rhsr 1,
                    mask32 rhsr 0,
                    ConstI32 1
                  ],
                callImportReturnTypes = [I32]
              }
            CallImport
              { target' = "__asterius_quotrem2_remainder",
                operands =
                  [ mask32 lhsHir 1,
                    mask32 lhsHir 0,
                    mask32 lhsLor 1,
                    mask32 lhsLor 0,
                    mask32 rhsr 1,
                    mask32 rhsr 0,
                    ConstI32 0
                  ],
                callImportReturnTypes = [I32]
              }
        }
  pure [quotout, remout]
-- Unhandled: MO_BSwap W8
marshalCmmPrimCall (GHC.MO_BSwap GHC.W16) [r] [x] = do
  lr <- marshalTypedCmmLocalReg r I32
  xe <- marshalAndCheckCmmExpr x I32
  pure
    [ UnresolvedSetLocal
        { unresolvedLocalReg = lr,
          value =
            Call
            { target = "hs_bswap16",
              operands = [xe],
              callReturnTypes = [I32],
              callHint = Just ([NoHint], [NoHint])
            }
        }
    ]
marshalCmmPrimCall (GHC.MO_BSwap GHC.W32) [r] [x] = do
  lr <- marshalTypedCmmLocalReg r I32
  xe <- marshalAndCheckCmmExpr x I32
  pure
    [ UnresolvedSetLocal
        { unresolvedLocalReg = lr,
          value =
            Call
            { target = "hs_bswap32",
              operands = [xe],
              callReturnTypes = [I32],
              callHint = Just ([NoHint], [NoHint])
            }
        }
    ]
marshalCmmPrimCall (GHC.MO_BSwap GHC.W64) [r] [x] = do
  lr <- marshalTypedCmmLocalReg r I64
  xe <- marshalAndCheckCmmExpr x I64
  pure
    [ UnresolvedSetLocal
        { unresolvedLocalReg = lr,
          value =
            Call
            { target = "hs_bswap64",
              operands = [xe],
              callReturnTypes = [I64],
              callHint = Just ([NoHint], [NoHint])
            }
        }
    ]
-- Atomic operations
marshalCmmPrimCall (GHC.MO_AtomicRMW GHC.W32 amop) [dst] [addr, n] =
  marshalCmmAtomicMachOpPrimCall amop dst addr n
marshalCmmPrimCall (GHC.MO_AtomicRead GHC.W32) [dst] [addr] = do
  dstr <- marshalTypedCmmLocalReg dst I32
  addrr <- fst <$> marshalCmmExpr addr
  pure
    [ UnresolvedSetLocal
        { unresolvedLocalReg = dstr,
          value =
            Load
              { signed = False,
                bytes = 4,
                offset = 0,
                valueType = I32,
                ptr = addrr
              }
        }
    ]
marshalCmmPrimCall (GHC.MO_AtomicWrite GHC.W32) [] [addr, val] = do
  addrr <- fst <$> marshalCmmExpr addr
  valr <- fst <$> marshalCmmExpr val
  pure
    [ Store
        { bytes = 4,
          offset = 0,
          ptr = addrr,
          value = valr,
          valueType = I32
        }
    ]
marshalCmmPrimCall (GHC.MO_Cmpxchg GHC.W32) [dst] [addr, oldv, newv] = do
  -- Copied from GHC.Prim:
  --
  -- Given an array, an offset in Int units, the expected old value, and
  -- the new value, perform an atomic compare and swap i.e. write the new
  -- value if the current value matches the provided old value. Returns
  -- the value of the element before the operation. Implies a full memory
  -- barrier.
  dstr <- marshalTypedCmmLocalReg dst I32
  addrr <- fst <$> marshalCmmExpr addr
  oldr <- fst <$> marshalCmmExpr oldv
  newr <- fst <$> marshalCmmExpr newv
  let expr1 =
        UnresolvedSetLocal
          { unresolvedLocalReg = dstr,
            value =
              Load
                { signed = False, -- in Cmm everything is unsigned
                  bytes = 4,
                  offset = 0, -- StgCmmPrim.doAtomicRMW has done the work
                  valueType = I32,
                  ptr = addrr
                }
          }
  let expr2 =
        If
          { condition = UnresolvedGetLocal dstr `eqInt32` oldr,
            ifTrue =
              Store
                { bytes = 4,
                  offset = 0,
                  ptr = addrr,
                  value = newr,
                  valueType = I32
                },
            ifFalse = Nothing
          }
  pure [expr1, expr2]
-- Uncovered cases
marshalCmmPrimCall op rs xs =
  throwM $ UnsupportedTodo

-- | Marshal an atomic MachOp.
marshalCmmAtomicMachOpPrimCall ::
  -- | The atomic machop to marshal
  GHC.AtomicMachOp ->
  -- | The destination register
  GHC.LocalReg ->
  -- | The address
  GHC.CmmExpr ->
  -- | The second operand (I32)
  GHC.CmmExpr ->
  CodeGen [Expression]
marshalCmmAtomicMachOpPrimCall machop dst addr n = do
  dstr <- marshalTypedCmmLocalReg dst I32
  addrr <- fst <$> marshalCmmExpr addr
  nr <- fst <$> marshalCmmExpr n
  let fn = case machop of
        GHC.AMO_Add -> addInt32
        GHC.AMO_Sub -> subInt32
        GHC.AMO_And -> andInt32
        GHC.AMO_Nand -> \e1 e2 -> xorInt32 (constI32 0xFFFFFFFF) $ andInt32 e1 e2
        GHC.AMO_Or -> orInt32
        GHC.AMO_Xor -> xorInt32
  let expr1 =
        UnresolvedSetLocal
          { unresolvedLocalReg = dstr,
            value =
              Load
                { signed = False, -- in Cmm everything is unsigned
                  bytes = 4,
                  offset = 0, -- StgCmmPrim.doAtomicRMW has done the work
                  valueType = I32,
                  ptr = addrr
                }
          }
  let expr2 =
        Store
          { bytes = 4,
            offset = 0,
            ptr = addrr,
            value = fn (UnresolvedGetLocal dstr) nr,
            valueType = I32
          }
  pure [expr1, expr2]

marshalCmmUnsafeCall ::
  GHC.CmmExpr ->
  GHC.ForeignConvention ->
  [GHC.LocalReg] ->
  [GHC.CmmExpr] ->
  CodeGen [Expression]
marshalCmmUnsafeCall p@(GHC.CmmLit (GHC.CmmLabel clbl)) f@(GHC.ForeignConvention _ xs_hint rs_hint _ _ _) rs xs = do
  sym <- marshalCLabel clbl
  xes <- for xs $ \x -> do
    (xe, _) <- marshalCmmExpr x
    pure xe
  case rs of
    [] ->
      pure
        [ Call {target = sym, operands = xes, callReturnTypes = [], callHint = Just (map marshalForeignHint xs_hint, map marshalForeignHint rs_hint)}
        ]
    [r] -> do
      (lr, vt) <- marshalCmmLocalReg r
      pure
        [ UnresolvedSetLocal
            { unresolvedLocalReg = lr,
              value =
                Call
                { target = sym,
                  operands = xes,
                  callReturnTypes = [vt],
                  callHint = Just (map marshalForeignHint xs_hint, map marshalForeignHint rs_hint)
                }
            }
        ]
    _ ->
      throwM $ UnsupportedCmmInstr $ showBS $
        GHC.CmmUnsafeForeignCall
          (GHC.ForeignTarget p f)
          rs
          xs
marshalCmmUnsafeCall p f rs xs = do
  fp <- marshalAndCheckCmmExpr p I32
  (xes, xts) <- unzip <$> for xs marshalCmmExpr
  case rs of
    [] ->
      pure
        [ CallIndirect
            { indirectTarget = fp,
              operands = xes,
              functionType = FunctionType {paramTypes = xts, returnTypes = []}
            }
        ]
    [r] -> do
      (lr, vt) <- marshalCmmLocalReg r
      pure
        [ UnresolvedSetLocal
            { unresolvedLocalReg = lr,
              value =
                CallIndirect
                  { indirectTarget = fp,
                    operands = xes,
                    functionType =
                      FunctionType
                        { paramTypes = xts,
                          returnTypes = [vt]
                        }
                  }
            }
        ]
    _ ->
      throwM $ UnsupportedCmmInstr $ showBS $
        GHC.CmmUnsafeForeignCall
          (GHC.ForeignTarget p f)
          rs
          xs

marshalCmmInstr :: GHC.CmmNode GHC.O GHC.O -> CodeGen [Expression]
marshalCmmInstr instr = case instr of
  GHC.CmmComment {} -> pure []
  GHC.CmmTick {} -> pure []
  GHC.CmmUnsafeForeignCall (GHC.PrimTarget op) rs xs ->
    marshalCmmPrimCall op rs xs
  GHC.CmmUnsafeForeignCall (GHC.ForeignTarget t c) rs xs ->
    marshalCmmUnsafeCall t c rs xs
  GHC.CmmAssign (GHC.CmmLocal r) e -> do
    (lr, vt) <- marshalCmmLocalReg r
    v <- marshalAndCheckCmmExpr e vt
    pure [UnresolvedSetLocal {unresolvedLocalReg = lr, value = v}]
  GHC.CmmAssign (GHC.CmmGlobal r) e -> do
    gr <- marshalCmmGlobalReg r
    v <- marshalAndCheckCmmExpr e $ unresolvedGlobalRegType gr
    pure [unresolvedSetGlobal gr v]
  GHC.CmmStore p e -> do
    pv <- marshalAndCheckCmmExpr p I32
    (dflags, _) <- ask
    store_instr <-
      join $
        dispatchAllCmmWidth
          (GHC.cmmExprWidth dflags e)
          ( do
              xe <- marshalAndCheckCmmExpr e I32
              pure Store
                { bytes = 1,
                  offset = 0,
                  ptr = pv,
                  value = xe,
                  valueType = I32
                }
          )
          ( do
              xe <- marshalAndCheckCmmExpr e I32
              pure Store
                { bytes = 2,
                  offset = 0,
                  ptr = pv,
                  value = xe,
                  valueType = I32
                }
          )
          ( do
              (xe, vt) <- marshalCmmExpr e
              pure Store
                { bytes = 4,
                  offset = 0,
                  ptr = pv,
                  value = xe,
                  valueType = vt
                }
          )
          ( do
              (xe, vt) <- marshalCmmExpr e
              pure Store
                { bytes = 8,
                  offset = 0,
                  ptr = pv,
                  value = xe,
                  valueType = vt
                }
          )
    pure [store_instr]
  _ -> throwM $ UnsupportedCmmInstr $ showBS instr

marshalCmmBlockBody :: [GHC.CmmNode GHC.O GHC.O] -> CodeGen [Expression]
marshalCmmBlockBody instrs = concat <$> for instrs marshalCmmInstr

-- | Flag determining whether we need to add an @__asterius_unreachable@ block.
newtype NeedsUnreachableBlock = NeedsUnreachableBlock Bool
  deriving (Semigroup, Monoid) via (Data.Monoid.Any)

marshalCmmBlockBranch ::
  GHC.CmmNode GHC.O GHC.C ->
  CodeGen ([Expression], Maybe Expression, [RelooperAddBranch], NeedsUnreachableBlock)
marshalCmmBlockBranch instr = case instr of
  GHC.CmmBranch lbl -> do
    k <- marshalLabel lbl
    pure
      ( [],
        Nothing,
        [AddBranch {to = k, addBranchCondition = Nothing}],
        NeedsUnreachableBlock False
      )
  GHC.CmmCondBranch {..} -> do
    c <- marshalAndCheckCmmExpr cml_pred I32
    kf <- marshalLabel cml_false
    kt <- marshalLabel cml_true
    pure
      ( [],
        Nothing,
        [AddBranch {to = kt, addBranchCondition = Just c} | kt /= kf]
          <> [AddBranch {to = kf, addBranchCondition = Nothing}],
        NeedsUnreachableBlock False
      )
  GHC.CmmSwitch cml_arg st -> do
    a <- marshalAndCheckCmmExpr cml_arg I32
    brs <- for (GHC.switchTargetsCases st) $ \(idx, lbl) -> do
      dest <- marshalLabel lbl
      pure (dest, [fromIntegral $ idx - fst (GHC.switchTargetsRange st)])
    (needs_unreachable, dest_def) <- case GHC.switchTargetsDefault st of
      Just lbl -> do
        klbl <- marshalLabel lbl
        return (NeedsUnreachableBlock False, klbl)
      Nothing -> pure (NeedsUnreachableBlock True, "__asterius_unreachable")
    pure
      ( [],
        Just $ case GHC.switchTargetsRange st of
              (0, _) -> a
              (l, _) -> Binary
                { binaryOp = SubInt32,
                  operand0 = a,
                  operand1 = ConstI32 $ fromIntegral l
                },
        [ AddBranchForSwitch {to = dest, indexes = tags}
          | (dest, tags) <- M.toList $ M.fromListWith (<>) brs,
            dest /= dest_def
        ]
          <> [AddBranch {to = dest_def, addBranchCondition = Nothing}],
        needs_unreachable
      )
  GHC.CmmCall {..} -> do
    t <- marshalAndCheckCmmExpr cml_target I32
    pure
      ( [ case t of
            Symbol {..} -> ReturnCall {returnCallTarget = unresolvedSymbol}
            _ -> ReturnCallIndirect {returnCallIndirectTarget = t}
        ],
        Nothing,
        [],
        NeedsUnreachableBlock False
      )
  _ -> throwM $ UnsupportedCmmBranch $ showBS instr

marshalCmmBlock ::
  [GHC.CmmNode GHC.O GHC.O] ->
  GHC.CmmNode GHC.O GHC.C ->
  CodeGen (RelooperBlock, NeedsUnreachableBlock)
marshalCmmBlock inner_nodes exit_node = do
  inner_exprs <- marshalCmmBlockBody inner_nodes
  (br_helper_exprs, maybe_switch_cond_expr, br_branches, needs_unreachable) <-
    marshalCmmBlockBranch exit_node
  pure $ case maybe_switch_cond_expr of
    Just switch_cond_expr ->
      ( RelooperBlock
          { addBlock =
              AddBlockWithSwitch
                { code = concatExpressions $ inner_exprs <> br_helper_exprs,
                  condition = switch_cond_expr
                },
            addBranches = br_branches
          },
        needs_unreachable
      )
    _ ->
      ( RelooperBlock
          { addBlock =
              AddBlock
                { code = concatExpressions $ inner_exprs <> br_helper_exprs
                },
            addBranches = br_branches
          },
        needs_unreachable
      )
  where
    concatExpressions es = Block {name = "", bodys = es, blockReturnTypes = []}

marshalCmmProc :: GHC.CmmGraph -> CodeGen Function
marshalCmmProc GHC.CmmGraph {g_graph = GHC.GMany _ body _, ..} = do
  entry_k <- marshalLabel g_entry
  (needs_unreachable, rbs) <- do
    let fn ::
          NeedsUnreachableBlock ->
          (GHC.Label, GHC.Block GHC.CmmNode GHC.C GHC.C) ->
          CodeGen (NeedsUnreachableBlock, (BS.ByteString, RelooperBlock))
        fn needs_unreachable_acc (lbl, GHC.BlockCC _ inner_nodes exit_node) = do
          k <- marshalLabel lbl
          (b, needs_unreachable) <- marshalCmmBlock (GHC.blockToList inner_nodes) exit_node
          pure (needs_unreachable_acc <> needs_unreachable, (k, b))
    mapAccumLM fn mempty (GHC.bodyList body)
  let blocks_unresolved
        | coerce needs_unreachable =
          ("__asterius_unreachable", unreachableRelooperBlock) : rbs
        | otherwise =
          rbs
  pure $ adjustLocalRegs Function
    { functionType = FunctionType {paramTypes = [], returnTypes = []},
      varTypes = [],
      body = CFG RelooperRun
        { entry = entry_k,
          blockMap = M.fromList blocks_unresolved,
          labelHelper = 0
        }
    }

marshalCmmDecl ::
  GHC.GenCmmDecl GHC.CmmStatics h GHC.CmmGraph -> CodeGen AsteriusModule
marshalCmmDecl decl = case decl of
  GHC.CmmData sec d@(GHC.Statics clbl _) -> do
    sym <- marshalCLabel clbl
    r <- marshalCmmData sym sec d
    pure $ mempty {staticsMap = SM.fromList [(sym, r)]}
  GHC.CmmProc _ clbl _ g -> do
    sym <- marshalCLabel clbl
    catch (do
      r <- marshalCmmProc g
      pure $ mempty {functionMap = SM.singleton sym r})
      (\(err :: AsteriusCodeGenError) -> case err of
        UnsupportedTodo -> pure mempty
        _ -> do
          cmm_str <- liftIO $ prettyShow g
          error $ cmm_str <> "\n" <> show err)


marshalHaskellIR :: GHC.Module -> [GHC.SptEntry] -> CmmIR -> CodeGen AsteriusModule
marshalHaskellIR this_mod spt_entries CmmIR {..} = do
  (dflags, _) <- ask
  let spt_map =
        SM.fromList
          [ (sym, (w0, w1))
            | GHC.SptEntry (idClosureSymbol dflags -> sym) (Fingerprint w0 w1) <-
                spt_entries
          ]
  r <- marshalRawCmm this_mod cmmRaw
  pure r {sptMap = spt_map}

marshalCmmIR :: GHC.Module -> CmmIR -> CodeGen AsteriusModule
marshalCmmIR this_mod CmmIR {..} = marshalRawCmm this_mod cmmRaw

marshalRawCmm ::
  GHC.Module ->
  Stream IO GHC.RawCmmGroup () ->
  CodeGen AsteriusModule
marshalRawCmm _ = w mempty
  where
    w m cmms = do
      r <- liftIO $ Stream.runStream cmms
      case r of
        Right (cmm_decls, cmms') -> do
          m' <-
            foldlM
              (\x cmm_decl -> (<> x) <$> marshalCmmDecl cmm_decl)
              m
              cmm_decls
          w m' cmms'
        _ -> pure m

{-
Note [unreachableRelooperBlock]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, we represent runtime failures using @Barf@s, and that's what
functions @relooper@ and @marshalCmmProc@ used to do as well, when @CmmSwitch@
was deemed to be non-exhaustive (lacking a default clause). But, we could do
better: GHC emits @CmmSwitch@es without a default clause only if it knows that
the match is indeed exhaustive and (so 'unreachableRelooperBlock' is really
unreachable). So, now both @marshalCmmProc@ and @relooper@ use @Unreachable@
directly, saving us some generated binary size (see issue #592).
-}
