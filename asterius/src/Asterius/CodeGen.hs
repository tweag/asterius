{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.CodeGen
  ( marshalCLabel
  , marshalToModuleSymbol
  , modulePath
  , moduleSymbolPath
  , marshalCmmDecl
  , marshalHaskellIR
  , marshalCmmIR
  , moduleSymbolDB
  ) where

import Asterius.Types
import qualified CLabel as GHC
import qualified Cmm as GHC
import qualified CmmSwitch as GHC
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Short as SBS
import Data.Coerce
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.String (fromString)
import Data.Traversable
import qualified Data.Vector as V
import Data.Word
import qualified GhcPlugins as GHC
import qualified Hoopl.Block as GHC
import qualified Hoopl.Graph as GHC
import qualified Hoopl.Label as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Orphans.Show ()
import Language.WebAssembly.Analysis
import Language.WebAssembly.Internals
import Language.WebAssembly.Marshal
import Language.WebAssembly.Types
import System.FilePath
import qualified Unique as GHC
import UnliftIO
import UnliftIO.Directory

{-# INLINEABLE marshalToModuleSymbol #-}
marshalToModuleSymbol :: GHC.Module -> AsteriusModuleSymbol
marshalToModuleSymbol (GHC.Module u m) =
  AsteriusModuleSymbol
    { unitId = SBS.toShort $ GHC.fs_bs $ GHC.unitIdFS u
    , moduleName =
        V.fromList $
        map SBS.toShort $
        CBS.splitWith (== '.') $ GHC.fs_bs $ GHC.moduleNameFS m
    }

{-# INLINEABLE moduleSymbolPath #-}
moduleSymbolPath ::
     MonadIO m => FilePath -> AsteriusModuleSymbol -> FilePath -> m FilePath
moduleSymbolPath topdir AsteriusModuleSymbol {..} ext = do
  createDirectoryIfMissing True $ takeDirectory p
  pure p
  where
    f = CBS.unpack . SBS.fromShort
    p =
      topdir </> f unitId </>
      V.foldr'
        (\c tot -> f c </> tot)
        (f (V.last moduleName) <.> ext)
        (V.init moduleName)

{-# INLINEABLE modulePath #-}
modulePath :: MonadIO m => FilePath -> GHC.Module -> FilePath -> m FilePath
modulePath topdir m = moduleSymbolPath topdir (marshalToModuleSymbol m)

{-# INLINEABLE marshalCLabel #-}
marshalCLabel :: GHC.DynFlags -> GHC.CLabel -> UnresolvedSymbol
marshalCLabel dflags =
  (coerce :: SBS.ShortByteString -> UnresolvedSymbol) .
  fromString . GHC.showSDoc dflags . GHC.pprCode GHC.AsmStyle . GHC.ppr

{-#INLINEABLE marshalLabel#-}
marshalLabel :: GHC.DynFlags -> GHC.Label -> SBS.ShortByteString
marshalLabel dflags =
  coerce . marshalCLabel dflags . GHC.mkLocalBlockLabel . GHC.getUnique

{-# INLINEABLE marshalCmmType #-}
marshalCmmType :: GHC.CmmType -> ValueType
marshalCmmType t
  | GHC.b8 `GHC.cmmEqType_ignoring_ptrhood` t ||
      GHC.b16 `GHC.cmmEqType_ignoring_ptrhood` t ||
      GHC.b32 `GHC.cmmEqType_ignoring_ptrhood` t = I32
  | GHC.b64 `GHC.cmmEqType_ignoring_ptrhood` t = I64
  | GHC.f32 `GHC.cmmEqType_ignoring_ptrhood` t = F32
  | GHC.f64 `GHC.cmmEqType_ignoring_ptrhood` t = F64
  | otherwise = impureThrow $ UnsupportedCmmType $ fromString $ show t

marshalCmmStatic ::
     MonadIO m => GHC.DynFlags -> GHC.CmmStatic -> m AsteriusStatic
marshalCmmStatic dflags st =
  liftIO $
  case st of
    GHC.CmmStaticLit lit ->
      case lit of
        GHC.CmmInt x GHC.W8 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int8)
            else encodePrim (fromIntegral x :: Word8)
        GHC.CmmInt x GHC.W16 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int16)
            else encodePrim (fromIntegral x :: Word16)
        GHC.CmmInt x GHC.W32 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int32)
            else encodePrim (fromIntegral x :: Word32)
        GHC.CmmInt x GHC.W64 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int64)
            else encodePrim (fromIntegral x :: Word64)
        GHC.CmmFloat x GHC.W32 ->
          pure $ Serialized $ encodePrim (fromRational x :: Float)
        GHC.CmmFloat x GHC.W64 ->
          pure $ Serialized $ encodePrim (fromRational x :: Double)
        GHC.CmmLabel clbl -> pure $ UnresolvedStatic $ marshalCLabel dflags clbl
        GHC.CmmLabelOff clbl o ->
          pure $ UnresolvedOffStatic (marshalCLabel dflags clbl) o
        _ -> throwIO $ UnsupportedCmmLit $ fromString $ show lit
    GHC.CmmUninitialised s -> pure $ Uninitialized s
    GHC.CmmString s -> pure $ Serialized $ SBS.pack $ s ++ [0]

marshalCmmData ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.Section
  -> GHC.CmmStatics
  -> m AsteriusStatics
marshalCmmData dflags _ (GHC.Statics _ ss) =
  fmap (AsteriusStatics . V.fromList) $ for ss $ marshalCmmStatic dflags

marshalAndCastCmmExpr ::
     MonadIO m => GHC.DynFlags -> GHC.CmmExpr -> ValueType -> m Expression
marshalAndCastCmmExpr dflags expr new_vt = do
  (old_e, old_vt) <- marshalCmmExpr dflags expr
  case (# old_vt, new_vt #) of
    (# I32, I32 #) -> pure old_e
    (# I64, I32 #) -> pure Unary {unaryOp = WrapInt64, operand0 = old_e}
    (# I32, I64 #) -> pure Unary {unaryOp = ExtendSInt32, operand0 = old_e}
    (# I64, I64 #) -> pure old_e
    (# F32, F32 #) -> pure old_e
    (# F64, F64 #) -> pure old_e
    _ -> throwIO $ UnsupportedImplicitCasting old_e old_vt new_vt

marshalCmmExpr ::
     MonadIO m => GHC.DynFlags -> GHC.CmmExpr -> m (Expression, ValueType)
marshalCmmExpr dflags expr =
  case expr of
    GHC.CmmLit lit ->
      case lit of
        GHC.CmmInt x w
          | w `V.elem` [GHC.W8, GHC.W16, GHC.W32] ->
            pure (ConstI32 $ fromIntegral x, I32)
          | w == GHC.W64 -> pure (ConstI64 $ fromIntegral x, I64)
        GHC.CmmFloat x GHC.W32 -> pure (ConstF32 $ fromRational x, F32)
        GHC.CmmFloat x GHC.W64 -> pure (ConstF64 $ fromRational x, F64)
        GHC.CmmLabel clbl ->
          pure
            ( Language.WebAssembly.Types.Unresolved
                {unresolvedSymbol = marshalCLabel dflags clbl}
            , I64)
        GHC.CmmLabelOff clbl o ->
          pure
            ( Language.WebAssembly.Types.UnresolvedOff
                { unresolvedSymbol = marshalCLabel dflags clbl
                , offset = fromIntegral o
                }
            , I64)
        _ -> throwIO $ UnsupportedCmmLit $ fromString $ show lit
    GHC.CmmLoad addr t -> do
      let vt = marshalCmmType t
      p <- marshalAndCastCmmExpr dflags addr I64
      pure
        ( Load
            { signed = False
            , bytes = sizeOfValueType vt
            , offset = 0
            , align = 0
            , valueType = vt
            , ptr = Unary {unaryOp = WrapInt64, operand0 = p}
            }
        , vt)
    GHC.CmmReg (GHC.CmmLocal lr) -> do
      let (lr_k, lr_vt) = marshalCmmLocalReg lr
      pure
        (UnresolvedGetLocal {unresolvedIndex = lr_k, valueType = lr_vt}, lr_vt)
    GHC.CmmReg (GHC.CmmGlobal gr) -> do
      let (gr_k, gr_vt) = marshalCmmGlobalReg gr
      pure (GetGlobal {name = gr_k, valueType = gr_vt}, gr_vt)
    GHC.CmmRegOff (GHC.CmmLocal lr) o -> do
      let (lr_k, lr_vt) = marshalCmmLocalReg lr
      case lr_vt of
        I32 ->
          pure
            ( Binary
                { binaryOp = AddInt32
                , operand0 =
                    UnresolvedGetLocal
                      {unresolvedIndex = lr_k, valueType = lr_vt}
                , operand1 = ConstI32 (fromIntegral o)
                }
            , lr_vt)
        I64 ->
          pure
            ( Binary
                { binaryOp = AddInt64
                , operand0 =
                    UnresolvedGetLocal
                      {unresolvedIndex = lr_k, valueType = lr_vt}
                , operand1 = ConstI64 (fromIntegral o)
                }
            , lr_vt)
        _ -> throwIO $ UnsupportedCmmExpr $ fromString $ show expr
    GHC.CmmRegOff (GHC.CmmGlobal gr) o -> do
      let (gr_k, gr_vt) = marshalCmmGlobalReg gr
      case gr_vt of
        I32 ->
          pure
            ( Binary
                { binaryOp = AddInt32
                , operand0 = GetGlobal {name = gr_k, valueType = gr_vt}
                , operand1 = ConstI32 (fromIntegral o)
                }
            , gr_vt)
        I64 ->
          pure
            ( Binary
                { binaryOp = AddInt64
                , operand0 = GetGlobal {name = gr_k, valueType = gr_vt}
                , operand1 = ConstI64 (fromIntegral o)
                }
            , gr_vt)
        _ -> throwIO $ UnsupportedCmmExpr $ fromString $ show expr
    GHC.CmmMachOp (GHC.MO_Add GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = AddInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Add GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = AddInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_Sub GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = SubInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Sub GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = SubInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_Eq GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = EqInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Eq GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = EqInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Ne GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = NeInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Ne GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = NeInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Mul GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = MulInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Mul GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = MulInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_S_Quot GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = DivSInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Quot GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = DivSInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_S_Rem GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = RemSInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Rem GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = RemSInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_S_Neg GHC.W32) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      pure
        ( Binary {binaryOp = SubInt32, operand0 = ConstI32 0, operand1 = x'}
        , I32)
    GHC.CmmMachOp (GHC.MO_S_Neg GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      pure
        ( Binary {binaryOp = SubInt64, operand0 = ConstI64 0, operand1 = x'}
        , I64)
    GHC.CmmMachOp (GHC.MO_U_Quot GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = DivUInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Quot GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = DivUInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_U_Rem GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = RemUInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Rem GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = RemUInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_S_Ge GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = GeSInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Ge GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = GeSInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Le GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = LeSInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Le GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = LeSInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Gt GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = GtSInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Gt GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = GtSInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Lt GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = LtSInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Lt GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = LtSInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Ge GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = GeUInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Ge GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = GeUInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Le GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = LeUInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Le GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = LeUInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Gt GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = GtUInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Gt GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = GtUInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Lt GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = LtUInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Lt GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = LtUInt64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Add GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = AddFloat32, operand0 = x', operand1 = y'}, F32)
    GHC.CmmMachOp (GHC.MO_F_Add GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = AddFloat64, operand0 = x', operand1 = y'}, F64)
    GHC.CmmMachOp (GHC.MO_F_Sub GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = SubFloat32, operand0 = x', operand1 = y'}, F32)
    GHC.CmmMachOp (GHC.MO_F_Sub GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = SubFloat64, operand0 = x', operand1 = y'}, F64)
    GHC.CmmMachOp (GHC.MO_F_Neg GHC.W32) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      pure (Unary {unaryOp = NegFloat32, operand0 = x'}, F32)
    GHC.CmmMachOp (GHC.MO_F_Neg GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      pure (Unary {unaryOp = NegFloat64, operand0 = x'}, F64)
    GHC.CmmMachOp (GHC.MO_F_Mul GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = MulFloat32, operand0 = x', operand1 = y'}, F32)
    GHC.CmmMachOp (GHC.MO_F_Mul GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = MulFloat64, operand0 = x', operand1 = y'}, F64)
    GHC.CmmMachOp (GHC.MO_F_Quot GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = DivFloat32, operand0 = x', operand1 = y'}, F32)
    GHC.CmmMachOp (GHC.MO_F_Quot GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = DivFloat64, operand0 = x', operand1 = y'}, F64)
    GHC.CmmMachOp (GHC.MO_F_Eq GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = EqFloat32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Eq GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = EqFloat64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Ne GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = NeFloat32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Ne GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = NeFloat64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Ge GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = GeFloat32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Ge GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = GeFloat64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Le GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = LeFloat32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Le GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = LeFloat64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Gt GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = GtFloat32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Gt GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = GtFloat64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Lt GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      y' <- marshalAndCastCmmExpr dflags y F32
      pure (Binary {binaryOp = LtFloat32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_F_Lt GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      y' <- marshalAndCastCmmExpr dflags y F64
      pure (Binary {binaryOp = LtFloat64, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_And GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = AndInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_And GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = AndInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_Or GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = OrInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Or GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = OrInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_Xor GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = XorInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Xor GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = XorInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_Not GHC.W32) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      pure
        ( Binary
            {binaryOp = XorInt32, operand0 = x', operand1 = ConstI32 0xFFFFFFFF}
        , I32)
    GHC.CmmMachOp (GHC.MO_Not GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      pure
        ( Binary
            { binaryOp = XorInt64
            , operand0 = x'
            , operand1 = ConstI64 0xFFFFFFFFFFFFFFFF
            }
        , I64)
    GHC.CmmMachOp (GHC.MO_Shl GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = ShlInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_Shl GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = ShlInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_U_Shr GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = ShrUInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_U_Shr GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = ShrUInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_S_Shr GHC.W32) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure (Binary {binaryOp = ShrSInt32, operand0 = x', operand1 = y'}, I32)
    GHC.CmmMachOp (GHC.MO_S_Shr GHC.W64) [x, y] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure (Binary {binaryOp = ShrSInt64, operand0 = x', operand1 = y'}, I64)
    GHC.CmmMachOp (GHC.MO_SF_Conv GHC.W32 GHC.W32) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      pure (Unary {unaryOp = ConvertSInt32ToFloat32, operand0 = x'}, F32)
    GHC.CmmMachOp (GHC.MO_SF_Conv GHC.W32 GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      pure (Unary {unaryOp = ConvertSInt32ToFloat64, operand0 = x'}, F64)
    GHC.CmmMachOp (GHC.MO_SF_Conv GHC.W64 GHC.W32) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      pure (Unary {unaryOp = ConvertSInt64ToFloat32, operand0 = x'}, F32)
    GHC.CmmMachOp (GHC.MO_SF_Conv GHC.W64 GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      pure (Unary {unaryOp = ConvertSInt64ToFloat64, operand0 = x'}, F64)
    GHC.CmmMachOp (GHC.MO_FS_Conv GHC.W32 GHC.W32) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      pure (Unary {unaryOp = TruncSFloat32ToInt32, operand0 = x'}, I32)
    GHC.CmmMachOp (GHC.MO_FS_Conv GHC.W32 GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      pure (Unary {unaryOp = TruncSFloat32ToInt64, operand0 = x'}, I64)
    GHC.CmmMachOp (GHC.MO_FS_Conv GHC.W64 GHC.W32) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      pure (Unary {unaryOp = TruncSFloat64ToInt32, operand0 = x'}, I32)
    GHC.CmmMachOp (GHC.MO_FS_Conv GHC.W64 GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      pure (Unary {unaryOp = TruncSFloat64ToInt64, operand0 = x'}, I64)
    GHC.CmmMachOp (GHC.MO_SS_Conv _ GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      pure (Unary {unaryOp = ExtendSInt32, operand0 = x'}, I64)
    GHC.CmmMachOp (GHC.MO_SS_Conv GHC.W64 _) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      pure (Unary {unaryOp = WrapInt64, operand0 = x'}, I32)
    GHC.CmmMachOp (GHC.MO_UU_Conv _ GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I32
      pure (Unary {unaryOp = ExtendUInt32, operand0 = x'}, I64)
    GHC.CmmMachOp (GHC.MO_UU_Conv GHC.W64 _) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x I64
      pure (Unary {unaryOp = WrapInt64, operand0 = x'}, I32)
    GHC.CmmMachOp (GHC.MO_FF_Conv GHC.W32 GHC.W64) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x F32
      pure (Unary {unaryOp = PromoteFloat32, operand0 = x'}, F64)
    GHC.CmmMachOp (GHC.MO_FF_Conv GHC.W64 GHC.W32) [x] -> do
      x' <- marshalAndCastCmmExpr dflags x F64
      pure (Unary {unaryOp = DemoteFloat64, operand0 = x'}, F32)
    _ -> throwIO $ UnsupportedCmmExpr $ fromString $ show expr

marshalCmmInstr ::
     MonadIO m => GHC.DynFlags -> GHC.CmmNode GHC.O GHC.O -> m [Expression]
marshalCmmInstr dflags instr =
  case instr of
    GHC.CmmComment {} -> pure []
    GHC.CmmTick {} -> pure []
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget GHC.MO_F64_Fabs) [r] [e] -> do
      let (k, F64) = marshalCmmLocalReg r
      x <- marshalAndCastCmmExpr dflags e F64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value = Unary {unaryOp = AbsFloat64, operand0 = x}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget GHC.MO_F32_Fabs) [r] [e] -> do
      let (k, F32) = marshalCmmLocalReg r
      x <- marshalAndCastCmmExpr dflags e F32
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value = Unary {unaryOp = AbsFloat32, operand0 = x}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget GHC.MO_F64_Sqrt) [r] [e] -> do
      let (k, F64) = marshalCmmLocalReg r
      x <- marshalAndCastCmmExpr dflags e F64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value = Unary {unaryOp = SqrtFloat64, operand0 = x}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget GHC.MO_F32_Sqrt) [r] [e] -> do
      let (k, F32) = marshalCmmLocalReg r
      x <- marshalAndCastCmmExpr dflags e F32
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value = Unary {unaryOp = SqrtFloat32, operand0 = x}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_S_QuotRem GHC.W32)) [rx, ry] [x, y] -> do
      let (kx, I32) = marshalCmmLocalReg rx
          (ky, I32) = marshalCmmLocalReg ry
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = kx
            , value =
                Binary {binaryOp = DivSInt32, operand0 = x', operand1 = y'}
            }
        , UnresolvedSetLocal
            { unresolvedIndex = ky
            , value =
                Binary {binaryOp = RemSInt32, operand0 = x', operand1 = y'}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_S_QuotRem GHC.W64)) [rx, ry] [x, y] -> do
      let (kx, I64) = marshalCmmLocalReg rx
          (ky, I64) = marshalCmmLocalReg ry
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = kx
            , value =
                Binary {binaryOp = DivSInt64, operand0 = x', operand1 = y'}
            }
        , UnresolvedSetLocal
            { unresolvedIndex = ky
            , value =
                Binary {binaryOp = RemSInt64, operand0 = x', operand1 = y'}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_U_QuotRem GHC.W32)) [rx, ry] [x, y] -> do
      let (kx, I32) = marshalCmmLocalReg rx
          (ky, I32) = marshalCmmLocalReg ry
      x' <- marshalAndCastCmmExpr dflags x I32
      y' <- marshalAndCastCmmExpr dflags y I32
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = kx
            , value =
                Binary {binaryOp = DivUInt32, operand0 = x', operand1 = y'}
            }
        , UnresolvedSetLocal
            { unresolvedIndex = ky
            , value =
                Binary {binaryOp = RemUInt32, operand0 = x', operand1 = y'}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_U_QuotRem GHC.W64)) [rx, ry] [x, y] -> do
      let (kx, I64) = marshalCmmLocalReg rx
          (ky, I64) = marshalCmmLocalReg ry
      x' <- marshalAndCastCmmExpr dflags x I64
      y' <- marshalAndCastCmmExpr dflags y I64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = kx
            , value =
                Binary {binaryOp = DivUInt64, operand0 = x', operand1 = y'}
            }
        , UnresolvedSetLocal
            { unresolvedIndex = ky
            , value =
                Binary {binaryOp = RemUInt64, operand0 = x', operand1 = y'}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget GHC.MO_Touch) _ _ -> pure []
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_Prefetch_Data _)) _ _ ->
      pure []
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_Clz GHC.W32)) [r] [e] -> do
      let (k, I64) = marshalCmmLocalReg r
      x <- marshalAndCastCmmExpr dflags e I32
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value =
                Unary
                  { unaryOp = ExtendSInt32
                  , operand0 = Unary {unaryOp = ClzInt32, operand0 = x}
                  }
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_Clz GHC.W64)) [r] [e] -> do
      let (k, I64) = marshalCmmLocalReg r
      x <- marshalAndCastCmmExpr dflags e I64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value = Unary {unaryOp = ClzInt64, operand0 = x}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_Ctz GHC.W32)) [r] [e] -> do
      let (k, I64) = marshalCmmLocalReg r
      x <- marshalAndCastCmmExpr dflags e I32
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value =
                Unary
                  { unaryOp = ExtendSInt32
                  , operand0 = Unary {unaryOp = CtzInt32, operand0 = x}
                  }
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_Ctz GHC.W64)) [r] [e] -> do
      let (k, I64) = marshalCmmLocalReg r
      x <- marshalAndCastCmmExpr dflags e I64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value = Unary {unaryOp = CtzInt64, operand0 = x}
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_AtomicRMW GHC.W32 atomic_op)) [r] [p, v] -> do
      let (k, I32) = marshalCmmLocalReg r
      p' <- marshalAndCastCmmExpr dflags p I64
      v' <- marshalAndCastCmmExpr dflags v I32
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value =
                AtomicRMW
                  { atomicRMWOp =
                      case atomic_op of
                        GHC.AMO_Add -> AtomicRMWAdd
                        GHC.AMO_Sub -> AtomicRMWSub
                        GHC.AMO_And -> AtomicRMWAnd
                        GHC.AMO_Or -> AtomicRMWOr
                        GHC.AMO_Xor -> AtomicRMWXor
                        _ ->
                          impureThrow $
                          UnsupportedCmmInstr $ fromString $ show instr
                  , bytes = 4
                  , offset = 0
                  , ptr = Unary {unaryOp = WrapInt64, operand0 = p'}
                  , value = v'
                  , valueType = I32
                  }
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_AtomicRMW GHC.W64 atomic_op)) [r] [p, v] -> do
      let (k, I64) = marshalCmmLocalReg r
      p' <- marshalAndCastCmmExpr dflags p I64
      v' <- marshalAndCastCmmExpr dflags v I64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value =
                AtomicRMW
                  { atomicRMWOp =
                      case atomic_op of
                        GHC.AMO_Add -> AtomicRMWAdd
                        GHC.AMO_Sub -> AtomicRMWSub
                        GHC.AMO_And -> AtomicRMWAnd
                        GHC.AMO_Or -> AtomicRMWOr
                        GHC.AMO_Xor -> AtomicRMWXor
                        _ ->
                          impureThrow $
                          UnsupportedCmmInstr $ fromString $ show instr
                  , bytes = 8
                  , offset = 0
                  , ptr = Unary {unaryOp = WrapInt64, operand0 = p'}
                  , value = v'
                  , valueType = I64
                  }
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_AtomicRead GHC.W32)) [r] [p] -> do
      let (k, I32) = marshalCmmLocalReg r
      p' <- marshalAndCastCmmExpr dflags p I64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value =
                AtomicLoad
                  { bytes = 4
                  , offset = 0
                  , valueType = I32
                  , ptr = Unary {unaryOp = WrapInt64, operand0 = p'}
                  }
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_AtomicRead GHC.W64)) [r] [p] -> do
      let (k, I64) = marshalCmmLocalReg r
      p' <- marshalAndCastCmmExpr dflags p I64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value =
                AtomicLoad
                  { bytes = 8
                  , offset = 0
                  , valueType = I64
                  , ptr = Unary {unaryOp = WrapInt64, operand0 = p'}
                  }
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_AtomicWrite GHC.W32)) [] [p, v] -> do
      p' <- marshalAndCastCmmExpr dflags p I64
      v' <- marshalAndCastCmmExpr dflags v I32
      pure
        [ AtomicStore
            { bytes = 4
            , offset = 0
            , ptr = Unary {unaryOp = WrapInt64, operand0 = p'}
            , value = v'
            , valueType = I32
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_AtomicWrite GHC.W64)) [] [p, v] -> do
      p' <- marshalAndCastCmmExpr dflags p I64
      v' <- marshalAndCastCmmExpr dflags v I64
      pure
        [ AtomicStore
            { bytes = 8
            , offset = 0
            , ptr = Unary {unaryOp = WrapInt64, operand0 = p'}
            , value = v'
            , valueType = I64
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_Cmpxchg GHC.W32)) [r] [p, o, n] -> do
      let (k, I64) = marshalCmmLocalReg r
      p' <- marshalAndCastCmmExpr dflags p I64
      o' <- marshalAndCastCmmExpr dflags o I32
      n' <- marshalAndCastCmmExpr dflags n I32
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value =
                AtomicCmpxchg
                  { bytes = 4
                  , offset = 0
                  , ptr = p'
                  , expected = o'
                  , replacement = n'
                  , valueType = I32
                  }
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget (GHC.MO_Cmpxchg GHC.W64)) [r] [p, o, n] -> do
      let (k, I64) = marshalCmmLocalReg r
      p' <- marshalAndCastCmmExpr dflags p I64
      o' <- marshalAndCastCmmExpr dflags o I64
      n' <- marshalAndCastCmmExpr dflags n I64
      pure
        [ UnresolvedSetLocal
            { unresolvedIndex = k
            , value =
                AtomicCmpxchg
                  { bytes = 8
                  , offset = 0
                  , ptr = p'
                  , expected = o'
                  , replacement = n'
                  , valueType = I64
                  }
            }
        ]
    GHC.CmmUnsafeForeignCall (GHC.ForeignTarget f _) rs xs -> do
      es <- for xs $ marshalCmmExpr dflags
      let (r_vt, from_rhs) =
            case rs of
              [] -> (None, id)
              [r] ->
                let (k, vt) = marshalCmmLocalReg r
                 in ( vt
                    , \e -> UnresolvedSetLocal {unresolvedIndex = k, value = e})
              _ -> impureThrow $ UnsupportedCmmInstr $ fromString $ show instr
          os = V.fromList $ map fst es
      pure . from_rhs <$>
        case f of
          GHC.CmmLit (GHC.CmmLabel clbl) ->
            pure
              Call
                { target = marshalCLabel dflags clbl
                , operands = os
                , valueType = r_vt
                }
          _ -> impureThrow $ UnsupportedCmmInstr $ fromString $ show instr
    GHC.CmmAssign (GHC.CmmLocal r) e -> do
      let (k, _) = marshalCmmLocalReg r
      (v, _) <- marshalCmmExpr dflags e
      pure [UnresolvedSetLocal {unresolvedIndex = k, value = v}]
    GHC.CmmAssign (GHC.CmmGlobal r) e -> do
      let (k, _) = marshalCmmGlobalReg r
      (v, _) <- marshalCmmExpr dflags e
      pure [SetGlobal {name = k, value = v}]
    GHC.CmmStore dest expr -> do
      dest_v <- marshalAndCastCmmExpr dflags dest I64
      (expr_v, expr_vt) <- marshalCmmExpr dflags expr
      pure
        [ Store
            { bytes = sizeOfValueType expr_vt
            , offset = 0
            , align = 0
            , ptr = Unary {unaryOp = WrapInt64, operand0 = dest_v}
            , value = expr_v
            , valueType = expr_vt
            }
        ]
    _ -> throwIO $ UnsupportedCmmInstr $ fromString $ show instr

marshalCmmBlockBody ::
     MonadIO m
  => GHC.DynFlags
  -> [GHC.CmmNode GHC.O GHC.O]
  -> m RelooperAddBlock
marshalCmmBlockBody dflags instrs = do
  es <- fmap concat $ for instrs $ marshalCmmInstr dflags
  pure
    AddBlock
      { code =
          case es of
            [] -> Nop
            [e] -> e
            _ -> Block {name = "", bodys = V.fromList es, valueType = None}
      }

marshalCmmLocalReg :: GHC.LocalReg -> (Int, ValueType)
marshalCmmLocalReg (GHC.LocalReg u t) = (GHC.getKey u, marshalCmmType t)

marshalCmmGlobalReg :: GHC.GlobalReg -> (SBS.ShortByteString, ValueType)
marshalCmmGlobalReg r =
  case r of
    GHC.VanillaReg _ _ -> (k, I64)
    GHC.FloatReg _ -> (k, F32)
    GHC.DoubleReg _ -> (k, F64)
    GHC.Sp -> (k, I64)
    GHC.SpLim -> (k, I64)
    GHC.Hp -> (k, I64)
    GHC.HpLim -> (k, I64)
    GHC.HpAlloc -> (k, I64)
    GHC.GCEnter1 -> (k, I64)
    GHC.GCFun -> (k, I64)
    GHC.BaseReg -> (k, I64)
    _ -> impureThrow $ UnsupportedCmmGlobalReg $ fromString $ show r
  where
    k = fromString $ show r

marshalCmmBlock ::
     MonadIO m
  => GHC.DynFlags
  -> (SBS.ShortByteString, [GHC.CmmNode GHC.O GHC.O], GHC.CmmNode GHC.O GHC.C)
  -> m (SBS.ShortByteString, RelooperBlock)
marshalCmmBlock dflags (k, body, branch) = do
  _body <- marshalCmmBlockBody dflags body
  _br_result <- marshalCmmBlockBranch dflags branch
  case _br_result of
    Left _append_expr ->
      case _body of
        AddBlock {code = Nop} ->
          pure
            ( k
            , RelooperBlock
                {addBlock = AddBlock {code = _append_expr}, addBranches = []})
        AddBlock {code = Block bk es _} ->
          pure
            ( k
            , RelooperBlock
                { addBlock =
                    AddBlock {code = Block bk (es <> [_append_expr]) I64}
                , addBranches = []
                })
        AddBlock {..} ->
          pure
            ( k
            , RelooperBlock
                { addBlock =
                    AddBlock
                      { code =
                          Block
                            { name = ""
                            , bodys = [code, _append_expr]
                            , valueType = I64
                            }
                      }
                , addBranches = []
                })
        _ -> throwIO $ UnsupportedRelooperAddBlock _body
    Right _branch ->
      pure (k, RelooperBlock {addBlock = _body, addBranches = _branch})

marshalCmmBlockBranch ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.CmmNode GHC.O GHC.C
  -> m (Either Expression (V.Vector RelooperAddBranch))
marshalCmmBlockBranch dflags instr =
  case instr of
    GHC.CmmBranch lbl ->
      pure $
      Right
        [ AddBranch
            {to = marshalLabel dflags lbl, condition = Null, code = Null}
        ]
    GHC.CmmCondBranch {..} -> do
      c <- marshalAndCastCmmExpr dflags cml_pred I32
      pure $
        Right
          [ AddBranch
              {to = marshalLabel dflags cml_true, condition = c, code = Null}
          , AddBranch
              { to = marshalLabel dflags cml_false
              , condition = Null
              , code = Null
              }
          ]
    GHC.CmmSwitch cml_pred st -> do
      p <- marshalAndCastCmmExpr dflags cml_pred I64
      pure $
        Right $
        V.fromList
          [ AddBranch
            { to = marshalLabel dflags lbl
            , condition =
                Binary
                  { binaryOp = EqInt64
                  , operand0 = p
                  , operand1 = ConstI64 $ fromIntegral k
                  }
            , code = Null
            }
          | (k, lbl) <- GHC.switchTargetsCases st
          ] <>
        (case GHC.switchTargetsDefault st of
           Just lbl ->
             [ AddBranch
                 {to = marshalLabel dflags lbl, condition = Null, code = Null}
             ]
           _ -> [])
    GHC.CmmCall {..} -> do
      t <- marshalAndCastCmmExpr dflags cml_target I64
      pure $ Left Return {value = t}
    _ -> throwIO $ UnsupportedCmmBranch $ fromString $ show instr

marshalCmmProc ::
     MonadIO m => GHC.DynFlags -> GHC.CmmGraph -> m AsteriusFunction
marshalCmmProc dflags GHC.CmmGraph {g_graph = GHC.GMany _ body _, ..} = do
  rbs <-
    fmap HM.fromList $
    for
      [ (marshalLabel dflags k, GHC.blockToList inner_nodes, exit_node)
      | (k, GHC.BlockCC _ inner_nodes exit_node) <- GHC.bodyList body
      ] $
    marshalCmmBlock dflags
  pure
    AsteriusFunction
      { body =
          RelooperRun
            { entry = marshalLabel dflags g_entry
            , blockMap = rbs
            , labelHelper = Nothing
            }
      }

marshalCmmDecl ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.GenCmmDecl GHC.CmmStatics h GHC.CmmGraph
  -> m AsteriusModule
marshalCmmDecl dflags decl =
  liftIO $
  case decl of
    GHC.CmmData s d@(GHC.Statics clbl _) ->
      let k = marshalCLabel dflags clbl
       in do ess <- tryAnyDeep $ tryDeep $ marshalCmmData dflags s d
             case ess of
               Left err ->
                 pure $
                 AsteriusModule
                   mempty
                   [(k, UnhandledException $ fromString $ show err)]
                   mempty
                   mempty
               Right (Left err) ->
                 pure $ AsteriusModule mempty [(k, err)] mempty mempty
               Right (Right ss) ->
                 pure $ AsteriusModule [(k, ss)] mempty mempty mempty
    GHC.CmmProc _ clbl _ g ->
      let k = marshalCLabel dflags clbl
       in do er <- tryAnyDeep $ tryDeep $ marshalCmmProc dflags g
             case er of
               Left err ->
                 pure $
                 AsteriusModule
                   mempty
                   mempty
                   mempty
                   [(k, UnhandledException $ fromString $ show err)]
               Right (Left err) ->
                 pure $ AsteriusModule mempty mempty mempty [(k, err)]
               Right (Right f) ->
                 pure $ AsteriusModule mempty mempty [(k, f)] mempty

marshalHaskellIR :: MonadIO m => GHC.DynFlags -> HaskellIR -> m AsteriusModule
marshalHaskellIR dflags HaskellIR {..} =
  fmap mconcat $ for cmmRaw $ marshalCmmDecl dflags

marshalCmmIR :: MonadIO m => GHC.DynFlags -> CmmIR -> m AsteriusModule
marshalCmmIR dflags CmmIR {..} =
  fmap mconcat $ for cmmRaw $ marshalCmmDecl dflags

moduleSymbolDB :: AsteriusModuleSymbol -> AsteriusModule -> AsteriusSymbolDB
moduleSymbolDB mod_sym AsteriusModule {..} =
  AsteriusSymbolDB
    { symbolMap =
        HM.map
          (\ss ->
             AsteriusSymbolInfo
               { symbolKind = StaticsSymbol
               , symbolSource = mod_sym
               , symbolAvailable = True
               , symbolDirectDeps = collectUnresolvedSymbols ss
               })
          staticsMap <>
        HM.map
          (const
             AsteriusSymbolInfo
               { symbolKind = StaticsSymbol
               , symbolSource = mod_sym
               , symbolAvailable = False
               , symbolDirectDeps = mempty
               })
          staticsErrorMap <>
        HM.map
          (\f ->
             AsteriusSymbolInfo
               { symbolKind = FunctionSymbol
               , symbolSource = mod_sym
               , symbolAvailable = True
               , symbolDirectDeps = collectUnresolvedSymbols f
               })
          functionMap <>
        HM.map
          (const
             AsteriusSymbolInfo
               { symbolKind = FunctionSymbol
               , symbolSource = mod_sym
               , symbolAvailable = False
               , symbolDirectDeps = mempty
               })
          functionErrorMap
    }
