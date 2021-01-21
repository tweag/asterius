{-# LANGUAGE OverloadedStrings #-}

module Asterius.Foreign.SupportedTypes
  ( ffiValueTypeSigned,
    isAnyTy,
    isJSValTy,
    jsValModule,
    jsValTyConOccName,
    getFFIValueType,
  )
where

import Asterius.Types
import Data.Maybe
import qualified ErrUtils as GHC
import qualified GhcPlugins as GHC
import qualified RepType as GHC

ffiValueTypeSigned :: FFIValueType -> Bool
ffiValueTypeSigned FFIJSVal = False
ffiValueTypeSigned FFIBool = False
ffiValueTypeSigned FFILifted = False
ffiValueTypeSigned FFIUnlifted = False
ffiValueTypeSigned FFIInt = True
ffiValueTypeSigned FFIInt8 = True
ffiValueTypeSigned FFIInt16 = True
ffiValueTypeSigned FFIInt32 = True
ffiValueTypeSigned FFIInt64 = True
ffiValueTypeSigned FFIWord = False
ffiValueTypeSigned FFIWord8 = False
ffiValueTypeSigned FFIWord16 = False
ffiValueTypeSigned FFIWord32 = False
ffiValueTypeSigned FFIWord64 = False
ffiValueTypeSigned FFIAddr = False
ffiValueTypeSigned FFIFloat = True
ffiValueTypeSigned FFIDouble = True

getFFIValueType :: GHC.Type -> Either String FFIValueType
getFFIValueType ty
  | isJSValTy ty =
    pure FFIJSVal
  | Just tc <- GHC.tyConAppTyCon_maybe ty,
    tc == GHC.boolTyCon =
    pure FFIBool
  | isAnyTy ty =
    pure FFILifted
  | is_product_type,
    data_con_arity == 1,
    GHC.isPrimitiveType data_con_arg_ty1 =
    getFFIValueType data_con_arg_ty1
  | GHC.isPrimitiveType ty =
    case GHC.typePrimRep ty of
      [GHC.UnliftedRep] -> pure FFIUnlifted
      [GHC.IntRep] -> pure FFIInt
      [GHC.Int8Rep] -> pure FFIInt8
      [GHC.Int16Rep] -> pure FFIInt16
      [GHC.Int64Rep] -> pure FFIInt64
      [GHC.WordRep] -> pure FFIWord
      [GHC.Word8Rep] -> pure FFIWord8
      [GHC.Word16Rep] -> pure FFIWord16
      [GHC.Word64Rep] -> pure FFIWord64
      [GHC.AddrRep] -> pure FFIAddr
      [GHC.FloatRep] -> pure FFIFloat
      [GHC.DoubleRep] -> pure FFIDouble
      _ ->
        Left $
          "Asterius.Foreign.SupportedTypes.getFFIValueType: "
            <> GHC.showPpr GHC.unsafeGlobalDynFlags ty
  | otherwise =
    Left $
      "Asterius.Foreign.SupportedTypes.getFFIValueType: "
        <> GHC.showPpr GHC.unsafeGlobalDynFlags ty
  where
    maybe_product_type = GHC.splitDataProductType_maybe ty
    is_product_type = isJust maybe_product_type
    Just (_, _, data_con, data_con_arg_tys) = maybe_product_type
    data_con_arity = GHC.dataConSourceArity data_con
    (data_con_arg_ty1 : _) = data_con_arg_tys

isAnyTy :: GHC.Type -> Bool
isAnyTy ty = case GHC.tcSplitTyConApp_maybe ty of
  Just (tc, _) -> tc == GHC.anyTyCon
  Nothing -> False

isJSValTy :: GHC.Type -> Bool
isJSValTy = GHC.isValid . checkRepTyCon isJSValTyCon

isJSValTyCon :: GHC.TyCon -> GHC.Validity
isJSValTyCon tc
  | GHC.nameModule_maybe n
      == Just jsValModule
      && GHC.nameOccName n
      == jsValTyConOccName =
    GHC.IsValid
  | otherwise =
    GHC.NotValid $ GHC.text "isJSValTyCon: not JSVal TyCon"
  where
    n = GHC.tyConName tc

{-# NOINLINE jsValModule #-}
jsValModule :: GHC.Module
jsValModule = GHC.mkModule GHC.baseUnitId (GHC.mkModuleName "Asterius.Types.JSVal")

{-# NOINLINE jsValTyConOccName #-}
jsValTyConOccName :: GHC.OccName
jsValTyConOccName = GHC.mkTcOcc "JSVal"

checkRepTyCon :: (GHC.TyCon -> GHC.Validity) -> GHC.Type -> GHC.Validity
checkRepTyCon check_tc ty = case GHC.splitTyConApp_maybe ty of
  Just (tc, tys)
    | GHC.isNewTyCon tc ->
      GHC.NotValid
        (GHC.hang msg 2 (mk_nt_reason tc tys GHC.$$ nt_fix))
    | otherwise -> case check_tc tc of
      GHC.IsValid -> GHC.IsValid
      GHC.NotValid extra -> GHC.NotValid (msg GHC.$$ extra)
  Nothing ->
    GHC.NotValid (GHC.quotes (GHC.ppr ty) GHC.<+> GHC.text "is not a data type")
  where
    msg =
      GHC.quotes (GHC.ppr ty)
        GHC.<+> GHC.text "cannot be marshalled in a foreign call"
    mk_nt_reason tc tys
      | null tys =
        GHC.text "because its data constructor is not in scope"
      | otherwise =
        GHC.text "because the data constructor for"
          GHC.<+> GHC.quotes (GHC.ppr tc)
          GHC.<+> GHC.text "is not in scope"
    nt_fix =
      GHC.text "Possible fix: import the data constructor to bring it into scope"
