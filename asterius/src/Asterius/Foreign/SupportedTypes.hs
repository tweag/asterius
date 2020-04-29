{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Foreign.SupportedTypes
  ( ffiValueTypeSigned,
    getFFIValueType0,
    getFFIValueType1,
    ffiBoxedValueTypeList,
    isJSValTy,
  )
where

import Asterius.Types
import qualified Data.ByteString as BS
import qualified ErrUtils as GHC
import qualified GhcPlugins as GHC
import qualified PrelNames as GHC
import qualified RepType as GHC
import qualified TysPrim as GHC

ffiJSVal :: FFIValueType
ffiJSVal = FFIValueType {ffiValueTypeRep = FFIJSValRep, hsTyCon = "JSVal"}

ffiValueTypeSigned :: FFIValueType -> Bool
ffiValueTypeSigned FFIValueType {..} = case ffiValueTypeRep of
  FFILiftedRep -> False
  FFIUnliftedRep -> False
  FFIJSValRep -> False
  FFIIntRep -> True
  FFIWordRep -> False
  FFIAddrRep -> False
  FFIFloatRep -> True
  FFIDoubleRep -> True

getFFIValueTypeRep :: GHC.TyCon -> FFIValueTypeRep
getFFIValueTypeRep tc = case GHC.tyConPrimRep tc of
  [GHC.LiftedRep] -> FFILiftedRep
  [GHC.UnliftedRep] -> FFIUnliftedRep
  [GHC.IntRep] -> FFIIntRep
  [GHC.WordRep] -> FFIWordRep
  [GHC.AddrRep] -> FFIAddrRep
  [GHC.FloatRep] -> FFIFloatRep
  [GHC.DoubleRep] -> FFIDoubleRep
  _ -> error "Asterius.Foreign.SupportedTypes.getFFIValueTypeRep"

getFFIValueType0 :: Bool -> GHC.TyCon -> Maybe FFIValueType
getFFIValueType0 accept_prim norm_tc
  | GHC.isValid (isJSValTyCon norm_tc) = pure ffiJSVal
  | otherwise = GHC.lookupNameEnv ffi_valuetype_map0 (GHC.getName norm_tc)
  where
    ffi_valuetype_map0
      | accept_prim = ffiValueTypeMap0
      | otherwise = ffiBoxedValueTypeMap0

getFFIValueType1 :: Bool -> GHC.TyCon -> Maybe FFIValueType
getFFIValueType1 accept_prim norm_tc =
  GHC.lookupNameEnv
    ffi_valuetype_map1
    (GHC.getName norm_tc)
  where
    ffi_valuetype_map1
      | accept_prim = ffiValueTypeMap1
      | otherwise = ffiBoxedValueTypeMap1

ffiBoxedValueTypeList :: [FFIValueType]
ffiBoxedValueTypeList =
  ffiJSVal
    : [ vt
        | vt@FFIValueType {..} <-
            GHC.nameEnvElts ffiBoxedValueTypeMap0
              <> GHC.nameEnvElts ffiBoxedValueTypeMap1,
          not $ BS.null hsTyCon
      ]

ffiBoxedValueTypeMap0,
  ffiBoxedValueTypeMap1,
  ffiPrimValueTypeMap0,
  ffiPrimValueTypeMap1,
  ffiValueTypeMap0,
  ffiValueTypeMap1 ::
    GHC.NameEnv FFIValueType
ffiBoxedValueTypeMap0 =
  GHC.mkNameEnv
    [ ( GHC.getName GHC.anyTyCon,
        FFIValueType {ffiValueTypeRep = FFILiftedRep, hsTyCon = ""}
      ),
      ( GHC.charTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.charPrimTyCon,
            hsTyCon = "Char"
          }
      ),
      ( GHC.boolTyConName,
        FFIValueType {ffiValueTypeRep = FFIWordRep, hsTyCon = "Bool"}
      ),
      ( GHC.intTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.intPrimTyCon,
            hsTyCon = "Int"
          }
      ),
      ( GHC.int8TyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.intPrimTyCon,
            hsTyCon = "Int8"
          }
      ),
      ( GHC.int16TyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.intPrimTyCon,
            hsTyCon = "Int16"
          }
      ),
      ( GHC.int32TyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.intPrimTyCon,
            hsTyCon = "Int32"
          }
      ),
      ( GHC.int64TyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.intPrimTyCon,
            hsTyCon = "Int64"
          }
      ),
      ( GHC.wordTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.wordPrimTyCon,
            hsTyCon = "Word"
          }
      ),
      ( GHC.word8TyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.wordPrimTyCon,
            hsTyCon = "Word8"
          }
      ),
      ( GHC.word16TyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.wordPrimTyCon,
            hsTyCon = "Word16"
          }
      ),
      ( GHC.word32TyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.wordPrimTyCon,
            hsTyCon = "Word32"
          }
      ),
      ( GHC.word64TyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.wordPrimTyCon,
            hsTyCon = "Word64"
          }
      ),
      ( GHC.floatTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.floatPrimTyCon,
            hsTyCon = "Float"
          }
      ),
      ( GHC.doubleTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.doublePrimTyCon,
            hsTyCon = "Double"
          }
      )
    ]
ffiBoxedValueTypeMap1 =
  GHC.mkNameEnv
    [ ( GHC.ptrTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.addrPrimTyCon,
            hsTyCon = "Ptr"
          }
      ),
      ( GHC.funPtrTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.addrPrimTyCon,
            hsTyCon = "FunPtr"
          }
      ),
      ( GHC.stablePtrTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.stablePtrPrimTyCon,
            hsTyCon = "StablePtr"
          }
      )
    ]
ffiPrimValueTypeMap0 =
  GHC.mkNameEnv
    [ ( GHC.charPrimTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.charPrimTyCon,
            hsTyCon = ""
          }
      ),
      ( GHC.intPrimTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.intPrimTyCon,
            hsTyCon = ""
          }
      ),
      ( GHC.wordPrimTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.wordPrimTyCon,
            hsTyCon = ""
          }
      ),
      ( GHC.floatPrimTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.floatPrimTyCon,
            hsTyCon = ""
          }
      ),
      ( GHC.doublePrimTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.doublePrimTyCon,
            hsTyCon = ""
          }
      ),
      ( GHC.addrPrimTyConName,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.addrPrimTyCon,
            hsTyCon = ""
          }
      )
    ]
ffiPrimValueTypeMap1 =
  GHC.mkNameEnv
    [ ( GHC.getName GHC.stablePtrPrimTyCon,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.stablePtrPrimTyCon,
            hsTyCon = ""
          }
      ),
      ( GHC.getName GHC.stableNamePrimTyCon,
        FFIValueType
          { ffiValueTypeRep = getFFIValueTypeRep GHC.stableNamePrimTyCon,
            hsTyCon = ""
          }
      )
    ]
ffiValueTypeMap0 = ffiBoxedValueTypeMap0 `GHC.plusNameEnv` ffiPrimValueTypeMap0
ffiValueTypeMap1 = ffiBoxedValueTypeMap1 `GHC.plusNameEnv` ffiPrimValueTypeMap1

isJSValTy :: GHC.Type -> Bool
isJSValTy = GHC.isValid . checkRepTyCon isJSValTyCon

isJSValTyCon :: GHC.TyCon -> GHC.Validity
isJSValTyCon tc
  | (GHC.moduleName <$> GHC.nameModule_maybe n)
      == Just asteriusTypesJSValModuleName
      && GHC.nameOccName n
      == jsValTyConOccName =
    GHC.IsValid
  | otherwise =
    GHC.NotValid $ GHC.text "isJSValTyCon: not JSVal TyCon"
  where
    n = GHC.tyConName tc

{-# NOINLINE asteriusTypesJSValModuleName #-}
asteriusTypesJSValModuleName :: GHC.ModuleName
asteriusTypesJSValModuleName = GHC.mkModuleName "Asterius.Types.JSVal"

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
