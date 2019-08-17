{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Foreign.Internals
  ( parseFFIFunctionType
    )
where

import Asterius.Types
import qualified GhcPlugins as GHC
import qualified PrelNames as GHC
import qualified TyCoRep as GHC

ffiValueTypeMap0, ffiValueTypeMap1 :: GHC.NameEnv FFIValueType
ffiValueTypeMap0 =
  GHC.mkNameEnv
    [ ( GHC.charTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Char",
            signed = False
            }
        ),
      ( GHC.boolTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Bool",
            signed = False
            }
        ),
      ( GHC.intTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Int",
            signed = True
            }
        ),
      ( GHC.wordTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Word",
            signed = False
            }
        ),
      ( GHC.floatTyConName,
        FFI_VAL
          { ffiWasmValueType = F32,
            ffiJSValueType = F32,
            hsTyCon = "Float",
            signed = True
            }
        ),
      ( GHC.doubleTyConName,
        FFI_VAL
          { ffiWasmValueType = F64,
            ffiJSValueType = F64,
            hsTyCon = "Double",
            signed = True
            }
        )
      ]

ffiValueTypeMap1 =
  GHC.mkNameEnv
    [ ( GHC.ptrTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Ptr",
            signed = False
            }
        ),
      ( GHC.funPtrTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "FunPtr",
            signed = False
            }
        ),
      ( GHC.stablePtrTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "StablePtr",
            signed = False
            }
        )
      ]

parseFFIValueType :: GHC.Type -> GHC.Type -> Maybe FFIValueType
parseFFIValueType sig_ty norm_sig_ty = case (sig_ty, norm_sig_ty) of
  (GHC.TyConApp _ _, GHC.TyConApp norm_tc []) ->
    GHC.lookupNameEnv ffiValueTypeMap0 (GHC.getName norm_tc)
  (GHC.TyConApp tc _, GHC.TyConApp norm_tc [_])
    | take 2 (GHC.occNameString (GHC.getOccName tc))
        == "JS"
        && GHC.getName norm_tc
        == GHC.stablePtrTyConName ->
      pure FFI_JSVAL
    | otherwise ->
      GHC.lookupNameEnv ffiValueTypeMap1 (GHC.getName norm_tc)
  _ -> Nothing

parseFFIFunctionType :: GHC.Type -> GHC.Type -> Maybe FFIFunctionType
parseFFIFunctionType sig_ty norm_sig_ty = case (sig_ty, norm_sig_ty) of
  (GHC.FunTy t1 t2, GHC.FunTy norm_t1 norm_t2) -> do
    vt <- parseFFIValueType t1 norm_t1
    ft <- parseFFIFunctionType t2 norm_t2
    pure ft {ffiParamTypes = vt : ffiParamTypes ft}
  (GHC.TyConApp _ tys1, GHC.TyConApp norm_tc norm_tys1)
    | GHC.getName norm_tc == GHC.ioTyConName ->
      case (tys1, norm_tys1) of
        (_, [GHC.TyConApp u []])
          | u == GHC.unitTyCon ->
            pure
              FFIFunctionType
                { ffiParamTypes = [],
                  ffiResultTypes = [],
                  ffiInIO = True
                  }
        ([t1], [norm_t1]) -> do
          r <- parseFFIValueType t1 norm_t1
          pure
            FFIFunctionType
              { ffiParamTypes = [],
                ffiResultTypes = [r],
                ffiInIO = True
                }
        _ -> Nothing
  _ -> do
    r <- parseFFIValueType sig_ty norm_sig_ty
    pure
      FFIFunctionType
        { ffiParamTypes = [],
          ffiResultTypes = [r],
          ffiInIO = False
          }
