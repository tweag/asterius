{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.CodeGen
  ( AsteriusCodeGenError(..)
  , AsteriusStatics(..)
  , AsteriusFunction(..)
  , AsteriusModule(..)
  , chaseCLabel
  , marshalIR
  ) where

import qualified CLabel as GHC
import qualified Cmm as GHC
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Short as SBS
import Data.Data (Data)
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Serialize (Serialize)
import Data.String (fromString)
import Data.Traversable
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)
import qualified GhcPlugins as GHC
import qualified Hoopl.Block as GHC
import qualified Hoopl.Graph as GHC
import qualified Hoopl.Label as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Orphans.Show ()
import Language.WebAssembly.Internals
import Language.WebAssembly.NIR
import qualified Unique as GHC
import UnliftIO

data AsteriusCodeGenError
  = UnsupportedCmmLit SBS.ShortByteString
  | UnsupportedCmmInstr SBS.ShortByteString
  | UnsupportedCmmType SBS.ShortByteString
  | UnsupportedCmmGlobalReg SBS.ShortByteString
  | UnsupportedCmmExpr SBS.ShortByteString
  | UnhandledException SBS.ShortByteString
  deriving (Show, Generic, Data)

instance Serialize AsteriusCodeGenError

instance NFData AsteriusCodeGenError

instance Exception AsteriusCodeGenError

data AsteriusStatic
  = Unresolved SBS.ShortByteString
  | UnresolvedOff SBS.ShortByteString
                  Int
  | Uninitialized Int
  | Serialized SBS.ShortByteString
  deriving (Show, Generic, Data)

instance Serialize AsteriusStatic

instance NFData AsteriusStatic

newtype AsteriusStatics = AsteriusStatics
  { asteriusStatics :: V.Vector AsteriusStatic
  } deriving (Show, Generic, Data)

instance Serialize AsteriusStatics

instance NFData AsteriusStatics

data AsteriusFunction = AsteriusFunction
  { body :: RelooperRun
  , localRegMap :: HM.HashMap Int ValueType
  , globalRegMap :: HM.HashMap SBS.ShortByteString ValueType
  } deriving (Show, Generic, Data)

instance Serialize AsteriusFunction

data AsteriusModule = AsteriusModule
  { staticsMap :: HM.HashMap SBS.ShortByteString AsteriusStatics
  , staticErrorMap :: HM.HashMap SBS.ShortByteString AsteriusCodeGenError
  , functionMap :: HM.HashMap SBS.ShortByteString AsteriusFunction
  , functionErrorMap :: HM.HashMap SBS.ShortByteString AsteriusCodeGenError
  } deriving (Show, Generic, Data)

instance Serialize AsteriusModule

instance Semigroup AsteriusModule where
  AsteriusModule sm0 se0 fm0 fe0 <> AsteriusModule sm1 se1 fm1 fe1 =
    AsteriusModule (sm0 <> sm1) (se0 <> se1) (fm0 <> fm1) (fe0 <> fe1)

instance Monoid AsteriusModule where
  mempty = AsteriusModule mempty mempty mempty mempty

{-# INLINEABLE marshalCLabel #-}
marshalCLabel :: GHC.DynFlags -> GHC.CLabel -> SBS.ShortByteString
marshalCLabel dflags =
  fromString . GHC.showSDoc dflags . GHC.pprCode GHC.AsmStyle . GHC.ppr

{-#INLINEABLE marshalLabel#-}
marshalLabel :: GHC.DynFlags -> GHC.Label -> SBS.ShortByteString
marshalLabel dflags =
  marshalCLabel dflags . GHC.mkLocalBlockLabel . GHC.getUnique

{-# INLINEABLE chaseCLabel #-}
chaseCLabel :: GHC.CLabel -> Maybe GHC.Module
chaseCLabel = GHC.nameModule_maybe <=< GHC.hasHaskellName

{-# INLINEABLE marshalCmmType #-}
marshalCmmType :: MonadIO m => GHC.CmmType -> m (ValueType, BinaryenIndex)
marshalCmmType t
  | GHC.b8 `GHC.cmmEqType_ignoring_ptrhood` t ||
      GHC.b16 `GHC.cmmEqType_ignoring_ptrhood` t ||
      GHC.b32 `GHC.cmmEqType_ignoring_ptrhood` t = pure (I32, 4)
  | GHC.b64 `GHC.cmmEqType_ignoring_ptrhood` t = pure (I64, 8)
  | GHC.f32 `GHC.cmmEqType_ignoring_ptrhood` t = pure (F32, 4)
  | GHC.f64 `GHC.cmmEqType_ignoring_ptrhood` t = pure (F64, 8)
  | otherwise = throwIO $ UnsupportedCmmType $ fromString $ show t

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
        GHC.CmmLabel clbl ->
          pure $ Asterius.CodeGen.Unresolved $ marshalCLabel dflags clbl
        GHC.CmmLabelOff clbl o ->
          pure $ Asterius.CodeGen.UnresolvedOff (marshalCLabel dflags clbl) o
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

marshalCmmExpr ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.CmmExpr
  -> m ( Expression
       , ValueType
       , BinaryenIndex
       , HM.HashMap Int ValueType
       , HM.HashMap SBS.ShortByteString ValueType)
marshalCmmExpr dflags expr =
  case expr of
    GHC.CmmLit lit ->
      case lit of
        GHC.CmmInt x w
          | w `V.elem` [GHC.W8, GHC.W16, GHC.W32] ->
            pure (ConstI32 $ fromIntegral x, I32, 4, mempty, mempty)
          | w == GHC.W64 ->
            pure (ConstI64 $ fromIntegral x, I64, 8, mempty, mempty)
        GHC.CmmFloat x GHC.W32 ->
          pure (ConstF32 $ fromRational x, F32, 4, mempty, mempty)
        GHC.CmmFloat x GHC.W64 ->
          pure (ConstF64 $ fromRational x, F64, 8, mempty, mempty)
        GHC.CmmLabel clbl ->
          pure
            ( Language.WebAssembly.NIR.Unresolved
                {unresolvedLabel = marshalCLabel dflags clbl}
            , I64
            , 8
            , mempty
            , mempty)
        GHC.CmmLabelOff clbl o ->
          pure
            ( Language.WebAssembly.NIR.UnresolvedOff
                { unresolvedLabel = marshalCLabel dflags clbl
                , offset = fromIntegral o
                }
            , I64
            , 8
            , mempty
            , mempty)
        _ -> throwIO $ UnsupportedCmmLit $ fromString $ show lit
    GHC.CmmLoad addr t -> do
      (vt, vts) <- marshalCmmType t
      (p, I64, 8, p_lrs, p_grs) <- marshalCmmExpr dflags addr
      pure
        ( Load
            { signed = False
            , bytes = vts
            , offset = 0
            , align = 0
            , valueType = vt
            , ptr = p
            }
        , vt
        , vts
        , p_lrs
        , p_grs)
    GHC.CmmReg (GHC.CmmLocal lr) -> do
      (lr_k, lr_vt, lr_vts) <- marshalCmmLocalReg lr
      pure
        ( UnresolvedGetLocal {unresolvedIndex = lr_k, valueType = lr_vt}
        , lr_vt
        , lr_vts
        , [(lr_k, lr_vt)]
        , mempty)
    GHC.CmmReg (GHC.CmmGlobal gr) -> do
      (gr_k, gr_vt) <- marshalCmmGlobalReg gr
      pure
        ( GetGlobal {name = gr_k, valueType = gr_vt}
        , gr_vt
        , 8
        , mempty
        , [(gr_k, gr_vt)])
    GHC.CmmRegOff (GHC.CmmLocal lr) o -> do
      (lr_k, lr_vt, _) <- marshalCmmLocalReg lr
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
            , lr_vt
            , 4
            , [(lr_k, lr_vt)]
            , mempty)
        I64 ->
          pure
            ( Binary
                { binaryOp = AddInt64
                , operand0 =
                    UnresolvedGetLocal
                      {unresolvedIndex = lr_k, valueType = lr_vt}
                , operand1 = ConstI64 (fromIntegral o)
                }
            , lr_vt
            , 8
            , [(lr_k, lr_vt)]
            , mempty)
        _ -> throwIO $ UnsupportedCmmExpr $ fromString $ show expr
    GHC.CmmRegOff (GHC.CmmGlobal gr) o -> do
      (gr_k, gr_vt) <- marshalCmmGlobalReg gr
      case gr_vt of
        I32 ->
          pure
            ( Binary
                { binaryOp = AddInt32
                , operand0 = GetGlobal {name = gr_k, valueType = gr_vt}
                , operand1 = ConstI32 (fromIntegral o)
                }
            , gr_vt
            , 4
            , mempty
            , [(gr_k, gr_vt)])
        I64 ->
          pure
            ( Binary
                { binaryOp = AddInt64
                , operand0 = GetGlobal {name = gr_k, valueType = gr_vt}
                , operand1 = ConstI64 (fromIntegral o)
                }
            , gr_vt
            , 8
            , mempty
            , [(gr_k, gr_vt)])
        _ -> throwIO $ UnsupportedCmmExpr $ fromString $ show expr
    GHC.CmmMachOp (GHC.MO_Add GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = AddInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Add GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = AddInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Sub GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = SubInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Sub GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = SubInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Eq GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = EqInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Eq GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = EqInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Ne GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = NeInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Ne GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = NeInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Mul GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = MulInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Mul GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = MulInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Quot GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = DivSInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Quot GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = DivSInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Rem GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = RemSInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Rem GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = RemSInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Neg GHC.W32) [x] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Binary {binaryOp = SubInt32, operand0 = ConstI32 0, operand1 = x'}
        , I32
        , 4
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_S_Neg GHC.W64) [x] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Binary {binaryOp = SubInt64, operand0 = ConstI64 0, operand1 = x'}
        , I64
        , 8
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_U_Quot GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = DivUInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Quot GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = DivUInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Rem GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = RemUInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Rem GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = RemUInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Ge GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GeSInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Ge GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GeSInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Le GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LeSInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Le GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LeSInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Gt GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GtSInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Gt GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GtSInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Lt GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LtSInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Lt GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LtSInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Ge GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GeUInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Ge GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GeUInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Le GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LeUInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Le GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LeUInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Gt GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GtUInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Gt GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GtUInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Lt GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LtUInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Lt GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LtUInt64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Add GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = AddFloat32, operand0 = x', operand1 = y'}
        , F32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Add GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = AddFloat64, operand0 = x', operand1 = y'}
        , F64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Sub GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = SubFloat32, operand0 = x', operand1 = y'}
        , F32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Sub GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = SubFloat64, operand0 = x', operand1 = y'}
        , F64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Neg GHC.W32) [x] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure (Unary {unaryOp = NegFloat32, operand0 = x'}, F32, 4, x_lrs, x_grs)
    GHC.CmmMachOp (GHC.MO_F_Neg GHC.W64) [x] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure (Unary {unaryOp = NegFloat64, operand0 = x'}, F64, 8, x_lrs, x_grs)
    GHC.CmmMachOp (GHC.MO_F_Mul GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = MulFloat32, operand0 = x', operand1 = y'}
        , F32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Mul GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = MulFloat64, operand0 = x', operand1 = y'}
        , F64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Quot GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = DivFloat32, operand0 = x', operand1 = y'}
        , F32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Quot GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = DivFloat64, operand0 = x', operand1 = y'}
        , F64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Eq GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = EqFloat32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Eq GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = EqFloat64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Ne GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = NeFloat32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Ne GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = NeFloat64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Ge GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GeFloat32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Ge GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GeFloat64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Le GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LeFloat32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Le GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LeFloat64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Gt GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GtFloat32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Gt GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = GtFloat64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Lt GHC.W32) [x, y] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LtFloat32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_F_Lt GHC.W64) [x, y] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', F64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = LtFloat64, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_And GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = AndInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_And GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = AndInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Or GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = OrInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Or GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = OrInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Xor GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = XorInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Xor GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = XorInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Not GHC.W32) [x] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Binary
            {binaryOp = XorInt32, operand0 = x', operand1 = ConstI32 0xFFFFFFFF}
        , I32
        , 4
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_Not GHC.W64) [x] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Binary
            { binaryOp = XorInt64
            , operand0 = x'
            , operand1 = ConstI64 0xFFFFFFFFFFFFFFFF
            }
        , I64
        , 8
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_Shl GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = ShlInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_Shl GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = ShlInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Shr GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = ShrUInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_U_Shr GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = ShrUInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Shr GHC.W32) [x, y] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I32, 4, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = ShrSInt32, operand0 = x', operand1 = y'}
        , I32
        , 4
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_S_Shr GHC.W64) [x, y] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      (y', I64, 8, y_lrs, y_grs) <- marshalCmmExpr dflags y
      pure
        ( Binary {binaryOp = ShrSInt64, operand0 = x', operand1 = y'}
        , I64
        , 8
        , x_lrs <> y_lrs
        , x_grs <> y_grs)
    GHC.CmmMachOp (GHC.MO_SF_Conv GHC.W32 GHC.W32) [x] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Unary {unaryOp = ConvertSInt32ToFloat32, operand0 = x'}
        , F32
        , 4
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_SF_Conv GHC.W32 GHC.W64) [x] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Unary {unaryOp = ConvertSInt32ToFloat64, operand0 = x'}
        , F64
        , 8
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_SF_Conv GHC.W64 GHC.W32) [x] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Unary {unaryOp = ConvertSInt64ToFloat32, operand0 = x'}
        , F32
        , 4
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_SF_Conv GHC.W64 GHC.W64) [x] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Unary {unaryOp = ConvertSInt64ToFloat64, operand0 = x'}
        , F64
        , 8
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_FS_Conv GHC.W32 GHC.W32) [x] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Unary {unaryOp = TruncSFloat32ToInt32, operand0 = x'}
        , I32
        , 4
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_FS_Conv GHC.W32 GHC.W64) [x] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Unary {unaryOp = TruncSFloat32ToInt64, operand0 = x'}
        , I64
        , 8
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_FS_Conv GHC.W64 GHC.W32) [x] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Unary {unaryOp = TruncSFloat64ToInt32, operand0 = x'}
        , I32
        , 4
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_FS_Conv GHC.W64 GHC.W64) [x] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        ( Unary {unaryOp = TruncSFloat64ToInt64, operand0 = x'}
        , I64
        , 8
        , x_lrs
        , x_grs)
    GHC.CmmMachOp (GHC.MO_SS_Conv _ GHC.W64) [x] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure (Unary {unaryOp = ExtendSInt32, operand0 = x'}, I64, 8, x_lrs, x_grs)
    GHC.CmmMachOp (GHC.MO_SS_Conv GHC.W64 _) [x] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure (Unary {unaryOp = WrapInt64, operand0 = x'}, I32, 4, x_lrs, x_grs)
    GHC.CmmMachOp (GHC.MO_UU_Conv _ GHC.W64) [x] -> do
      (x', I32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure (Unary {unaryOp = ExtendUInt32, operand0 = x'}, I64, 8, x_lrs, x_grs)
    GHC.CmmMachOp (GHC.MO_UU_Conv GHC.W64 _) [x] -> do
      (x', I64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure (Unary {unaryOp = WrapInt64, operand0 = x'}, I32, 4, x_lrs, x_grs)
    GHC.CmmMachOp (GHC.MO_FF_Conv GHC.W32 GHC.W64) [x] -> do
      (x', F32, 4, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        (Unary {unaryOp = PromoteFloat32, operand0 = x'}, F64, 8, x_lrs, x_grs)
    GHC.CmmMachOp (GHC.MO_FF_Conv GHC.W64 GHC.W32) [x] -> do
      (x', F64, 8, x_lrs, x_grs) <- marshalCmmExpr dflags x
      pure
        (Unary {unaryOp = DemoteFloat64, operand0 = x'}, F32, 4, x_lrs, x_grs)
    _ -> throwIO $ UnsupportedCmmExpr $ fromString $ show expr

marshalCmmInstr ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.CmmNode GHC.O GHC.O
  -> m ( Expression
       , HM.HashMap Int ValueType
       , HM.HashMap SBS.ShortByteString ValueType)
marshalCmmInstr dflags instr =
  case instr of
    GHC.CmmAssign (GHC.CmmLocal r) e -> do
      (k, t, _) <- marshalCmmLocalReg r
      (v, _, _, lrs, grs) <- marshalCmmExpr dflags e
      pure
        ( UnresolvedSetLocal {unresolvedIndex = k, value = v}
        , [(k, t)] <> lrs
        , grs)
    GHC.CmmAssign (GHC.CmmGlobal r) e -> do
      (k, t) <- marshalCmmGlobalReg r
      (v, _, _, lrs, grs) <- marshalCmmExpr dflags e
      pure (SetGlobal {name = k, value = v}, lrs, [(k, t)] <> grs)
    GHC.CmmStore dest expr -> do
      (dest_v, I64, 8, dest_lrs, dest_grs) <- marshalCmmExpr dflags dest
      (expr_v, expr_vt, expr_vts, expr_lrs, expr_grs) <-
        marshalCmmExpr dflags expr
      pure
        ( Store
            { bytes = expr_vts
            , offset = 0
            , align = 0
            , ptr = Unary {unaryOp = WrapInt64, operand0 = dest_v}
            , value = expr_v
            , valueType = expr_vt
            }
        , dest_lrs <> expr_lrs
        , dest_grs <> expr_grs)
    _ -> throwIO $ UnsupportedCmmInstr $ fromString $ show instr

marshalCmmBlockBody ::
     MonadIO m
  => GHC.DynFlags
  -> [GHC.CmmNode GHC.O GHC.O]
  -> m ( RelooperAddBlock
       , HM.HashMap Int ValueType
       , HM.HashMap SBS.ShortByteString ValueType)
marshalCmmBlockBody dflags instrs = do
  (es, lrs, grs) <- fmap unzip3 $ for instrs $ marshalCmmInstr dflags
  pure
    ( AddBlock
        {code = Block {name = "", bodys = V.fromList es, valueType = None}}
    , mconcat lrs
    , mconcat grs)

marshalCmmLocalReg ::
     MonadIO m => GHC.LocalReg -> m (Int, ValueType, BinaryenIndex)
marshalCmmLocalReg (GHC.LocalReg u t) = do
  (vt, vts) <- marshalCmmType t
  pure (GHC.getKey u, vt, vts)

marshalCmmGlobalReg ::
     MonadIO m => GHC.GlobalReg -> m (SBS.ShortByteString, ValueType)
marshalCmmGlobalReg r =
  case r of
    GHC.VanillaReg _ _ -> pure (k, I64)
    GHC.FloatReg _ -> pure (k, F32)
    GHC.DoubleReg _ -> pure (k, F64)
    GHC.Sp -> pure (k, I64)
    GHC.SpLim -> pure (k, I64)
    GHC.Hp -> pure (k, I64)
    GHC.HpLim -> pure (k, I64)
    GHC.HpAlloc -> pure (k, I64)
    GHC.GCEnter1 -> pure (k, I64)
    GHC.GCFun -> pure (k, I64)
    GHC.BaseReg -> pure (k, I64)
    _ -> throwIO $ UnsupportedCmmGlobalReg $ fromString $ show r
  where
    k = fromString $ show r

marshalCmmBlock ::
     MonadIO m
  => GHC.DynFlags
  -> (SBS.ShortByteString, [GHC.CmmNode GHC.O GHC.O], GHC.CmmNode GHC.O GHC.C)
  -> m ( SBS.ShortByteString
       , RelooperBlock
       , HM.HashMap Int ValueType
       , HM.HashMap SBS.ShortByteString ValueType)
marshalCmmBlock dflags (k, body, branch) = do
  (_body, _body_lrs, _body_grs) <- marshalCmmBlockBody dflags body
  _branch <- marshalCmmBlockBranch dflags branch
  pure
    ( k
    , RelooperBlock {addBlock = _body, addBranches = _branch}
    , _body_lrs
    , _body_grs)

marshalCmmBlockBranch ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.CmmNode GHC.O GHC.C
  -> m (V.Vector RelooperAddBranch)
marshalCmmBlockBranch _ _ = pure []

marshalCmmProc ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.CmmGraph
  -> m ( RelooperRun
       , HM.HashMap Int ValueType
       , HM.HashMap SBS.ShortByteString ValueType)
marshalCmmProc dflags GHC.CmmGraph {g_graph = GHC.GMany _ body _, ..} = do
  rbs <-
    for
      [ (marshalLabel dflags k, GHC.blockToList inner_nodes, exit_node)
      | (k, GHC.BlockCC _ inner_nodes exit_node) <- GHC.bodyList body
      ] $
    marshalCmmBlock dflags
  pure
    ( RelooperRun
        { entry = marshalLabel dflags g_entry
        , blockMap = HM.fromList [(k, b) | (k, b, _, _) <- rbs]
        , labelHelper = Nothing
        }
    , mconcat [lr | (_, _, lr, _) <- rbs]
    , mconcat [gr | (_, _, _, gr) <- rbs])

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
               Right (Right (r, lrs, grs)) ->
                 pure $
                 AsteriusModule
                   mempty
                   mempty
                   [(k, AsteriusFunction r lrs grs)]
                   mempty

marshalIR :: MonadIO m => GHC.DynFlags -> IR -> m AsteriusModule
marshalIR dflags IR {..} = fmap mconcat $ for cmmRaw $ marshalCmmDecl dflags
