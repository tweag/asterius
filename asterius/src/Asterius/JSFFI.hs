{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.JSFFI
  ( getFFIModule,
    generateFFIFunctionImports,
    generateFFIImportObjectFactory,
  )
where

import Asterius.EDSL
import Asterius.Foreign.Internals
import Asterius.Foreign.SupportedTypes
import Asterius.Foreign.TypesTag
import Asterius.Passes.GlobalRegs
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import qualified CmmCallConv as GHC
import qualified CmmExpr as GHC
import qualified CmmNode as GHC
import Data.ByteString.Builder
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Constants
import qualified CmmCallConv as GHC

recoverWasmWrapperValueType :: FFIValueType -> ValueType
recoverWasmWrapperValueType FFIValueType {..} = case ffiValueTypeRep of
  FFILiftedRep -> I32
  FFIUnliftedRep -> I32
  FFIJSValRep -> I32
  FFIIntRep -> I32
  FFIInt8Rep -> I32
  FFIInt16Rep -> I32
  FFIInt32Rep -> I32
  FFIInt64Rep -> I64
  FFIWordRep -> I32
  FFIWord8Rep -> I32
  FFIWord16Rep -> I32
  FFIWord32Rep -> I32
  FFIWord64Rep -> I64
  FFIAddrRep -> I32
  FFIFloatRep -> F32
  FFIDoubleRep -> F64

recoverWasmImportFunctionType :: FFISafety -> FFIFunctionType -> FunctionType
recoverWasmImportFunctionType ffi_safety FFIFunctionType {..}
  | is_unsafe =
    FunctionType
      { paramTypes = param_types,
        returnTypes = ret_types
      }
  | otherwise = FunctionType {paramTypes = param_types, returnTypes = []}
  where
    is_unsafe = ffi_safety == FFIUnsafe
    param_types = map recoverWasmWrapperValueType ffiParamTypes
    ret_types = map recoverWasmWrapperValueType ffiResultTypes

recoverWasmWrapperFunctionType :: FFISafety -> FFIFunctionType -> FunctionType
recoverWasmWrapperFunctionType ffi_safety FFIFunctionType {..}
  | is_unsafe =
    FunctionType
      { paramTypes = param_types,
        returnTypes = ret_types
      }
  | otherwise = FunctionType {paramTypes = [], returnTypes = []}
  where
    is_unsafe = ffi_safety == FFIUnsafe
    param_types = map recoverWasmWrapperValueType ffiParamTypes
    ret_types = map recoverWasmWrapperValueType ffiResultTypes

getFFIModule :: GHC.DynFlags -> GHC.Module -> IO AsteriusModule
getFFIModule dflags ms_mod = do
  ffi_import_state <-
    atomicModifyIORef' globalFFIHookState $ \ffi_hook_state ->
      ( ffi_hook_state
          { ffiHookState = M.delete ms_mod $ ffiHookState ffi_hook_state
          },
        M.findWithDefault mempty ms_mod (ffiHookState ffi_hook_state)
      )
  pure $ generateFFIWrapperModule dflags ffi_import_state

generateImplicitCastExpression ::
  Bool -> [ValueType] -> [ValueType] -> Expression -> Expression
generateImplicitCastExpression signed src_ts dest_ts src_expr
      | src_ts == dest_ts =
        src_expr
      | otherwise =
        error $
          "Unsupported implicit cast from "
            <> show src_ts
            <> " to "
            <> show dest_ts

generateFFIImportWrapperFunction ::
  GHC.DynFlags -> EntitySymbol -> FFIImportDecl -> Function
generateFFIImportWrapperFunction dflags k imp_decl@FFIImportDecl {..}
  | is_unsafe =
    Function
      { functionType = wrapper_func_type,
        varTypes = [],
        body =
          generateImplicitCastExpression
            ( case ffiResultTypes ffiFunctionType of
                [ffi_vt] -> ffiValueTypeSigned ffi_vt
                _ -> False
            )
            (returnTypes import_func_type)
            (returnTypes wrapper_func_type)
            $ CallImport
              { target' = entityName k,
                operands =
                  [ generateImplicitCastExpression
                      (ffiValueTypeSigned param_t)
                      [wrapper_param_t]
                      [import_param_t]
                      GetLocal {index = i, valueType = wrapper_param_t}
                    | (i, param_t, wrapper_param_t, import_param_t) <-
                        zip4
                          [0 ..]
                          ffi_param_types
                          (paramTypes wrapper_func_type)
                          (paramTypes import_func_type)
                  ],
                callImportReturnTypes = returnTypes import_func_type
              }
      }
  | otherwise = asyncImportWrapper dflags k imp_decl
  where
    is_unsafe = ffiSafety == FFIUnsafe
    ffi_param_types = ffiParamTypes ffiFunctionType
    import_func_type = recoverWasmImportFunctionType FFIUnsafe ffiFunctionType
    wrapper_func_type = recoverWasmWrapperFunctionType FFIUnsafe ffiFunctionType

marshalParamLocation :: GHC.ParamLocation -> UnresolvedGlobalReg
marshalParamLocation (GHC.RegisterParam (GHC.VanillaReg i _)) = VanillaReg i
marshalParamLocation (GHC.RegisterParam (GHC.FloatReg i)) = FloatReg i
marshalParamLocation (GHC.RegisterParam (GHC.DoubleReg i)) = DoubleReg i
marshalParamLocation (GHC.RegisterParam (GHC.LongReg i)) = LongReg i
marshalParamLocation (GHC.RegisterParam x) = error $ "Asterius.JSFFI.marshalParamLocation: RegisterParam " <> show x
marshalParamLocation (GHC.StackParam x) = error $ "Asterius.JSFFI.marshalParamLocation: StackParam " <> show x

recoverCmmType :: GHC.DynFlags -> FFIValueType -> GHC.CmmType
recoverCmmType dflags FFIValueType {..} = case ffiValueTypeRep of
  FFILiftedRep -> GHC.gcWord dflags
  FFIUnliftedRep -> GHC.gcWord dflags
  FFIJSValRep -> GHC.gcWord dflags
  FFIIntRep -> GHC.bWord dflags
  FFIInt8Rep -> GHC.b8
  FFIInt16Rep -> GHC.b16
  FFIInt32Rep -> GHC.b32
  FFIInt64Rep -> GHC.b64
  FFIWordRep -> GHC.bWord dflags
  FFIWord8Rep -> GHC.b8
  FFIWord16Rep -> GHC.b16
  FFIWord32Rep -> GHC.b32
  FFIWord64Rep -> GHC.b64
  FFIAddrRep -> GHC.bWord dflags
  FFIFloatRep -> GHC.f32
  FFIDoubleRep -> GHC.f64

asyncImportWrapper ::
  GHC.DynFlags -> EntitySymbol -> FFIImportDecl -> Function
asyncImportWrapper dflags k FFIImportDecl {..} =
  Function
    { functionType = wrapper_func_type,
      varTypes = [],
      body =
        Block
          { name = "",
            bodys =
              [ CallImport
                  { target' = entityName k,
                    operands =
                      [ generateImplicitCastExpression
                          (ffiValueTypeSigned param_t)
                          [recoverWasmWrapperValueType param_t]
                          [import_param_t]
                          (unresolvedGetGlobal param_reg)
                        | (param_reg, param_t, import_param_t) <-
                            zip3
                              param_regs
                              ffi_param_types
                              (paramTypes import_func_type)
                      ],
                    callImportReturnTypes = []
                  },
                Store
                  { bytes = 4,
                    offset =
                      fromIntegral $
                        offset_Capability_r
                          + offset_StgRegTable_rRet,
                    ptr = mainCapability,
                    value = constI32 ret_ThreadBlocked,
                    valueType = I32
                  },
                Store
                  { bytes = 2,
                    offset = fromIntegral offset_StgTSO_why_blocked,
                    ptr = unresolvedGetGlobal CurrentTSO,
                    value = constI32 blocked_BlockedOnCCall,
                    valueType = I32
                  },
                ReturnCall
                  { returnCallTarget = "stg_returnToSchedNotPaused"
                  }
              ],
            blockReturnTypes = []
          }
    }
  where
    ffi_param_types = ffiParamTypes ffiFunctionType
    import_func_type = recoverWasmImportFunctionType FFISafe ffiFunctionType
    wrapper_func_type = recoverWasmWrapperFunctionType FFISafe ffiFunctionType
    (_, map (marshalParamLocation . snd) . sortOn (fst . fst) -> param_regs) =
      GHC.assignArgumentsPos
        dflags
        0
        GHC.NativeNodeCall
        snd
        (zip [(0 :: Int) ..] (map (recoverCmmType dflags) ffi_param_types))

generateFFIWrapperModule :: GHC.DynFlags -> FFIMarshalState -> AsteriusModule
generateFFIWrapperModule dflags mod_ffi_state@FFIMarshalState {..} =
  mempty
    { functionMap =
        SM.fromList
          [ (k <> "_wrapper", wrapper_func)
            | (k, wrapper_func) <- import_wrapper_funcs
          ],
      ffiMarshalState = mod_ffi_state
    }
  where
    import_wrapper_funcs =
      [ (k, generateFFIImportWrapperFunction dflags k ffi_decl)
        | (k, ffi_decl) <- SM.toList ffiImportDecls
      ]

generateFFIFunctionImports :: FFIMarshalState -> [FunctionImport]
generateFFIFunctionImports FFIMarshalState {..} =
  [ FunctionImport
      { internalName = entityName k,
        externalModuleName = "jsffi",
        externalBaseName = entityName k,
        functionType = recoverWasmImportFunctionType ffiSafety ffiFunctionType
      }
    | (k, FFIImportDecl {..}) <- SM.toList ffiImportDecls
  ]

-- | Generate FFI import lambda
--
-- Unsafe:
--    * Return a JSVal: (_1,_2,...,_N) => __asterius_jsffi.newJSValzh(code)
--    * Otherwise:      (_1,_2,...,_N) => (code)
-- Safe:
--    * (_1,_2,...,_N) => __asterius_jsffi.returnFFIPromise((async () => (code))().then(v => [returnTypes,v]))
--
-- Note: we use `(async () => (code))()` instead of `Promise.resolve(code)` to
-- handle `code` throwing synchronously instead of returning a rejected Promise.
generateFFIImportLambda :: FFIImportDecl -> Builder
generateFFIImportLambda FFIImportDecl {ffiFunctionType = FFIFunctionType {..}, ..}
  | is_unsafe =
    lamb <> unsafe_code
  | otherwise =
    lamb
      <> "__asterius_jsffi.returnFFIPromise("
      <> "(async () => "
      <> safe_code
      <> ")())"
  where
    is_unsafe = ffiSafety == FFIUnsafe
    lamb =
      "("
        <> mconcat
          ( intersperse
              ","
              ["$" <> intDec i | i <- [1 .. length ffiParamTypes]]
          )
        <> ")=>"
    getjsval_code =
      mconcat
        [ case ffiValueTypeRep pt of
            FFIJSValRep ->
              "$"
                <> intDec i
                <> " = __asterius_jsffi.getJSValzh($"
                <> intDec i
                <> ");"
            _ -> mempty
          | (i, pt) <- zip [1 ..] ffiParamTypes
        ]
    unsafe_code =
      "{"
        <> getjsval_code
        <> "return "
        <> ( case map ffiValueTypeRep ffiResultTypes of
               [FFIJSValRep] ->
                 "__asterius_jsffi.newJSValzh("
                   <> byteString ffiSourceText
                   <> ")"
               _ -> "(" <> byteString ffiSourceText <> ")"
           )
        <> ";}"
    safe_code =
      "{"
        <> getjsval_code
        <> "return ["
        <> intDec (ffiValueTypesTag ffiResultTypes)
        <> ", await ("
        <> byteString ffiSourceText
        <> ")];}"

generateFFIImportObjectFactory :: FFIMarshalState -> Builder
generateFFIImportObjectFactory FFIMarshalState {..} =
  "__asterius_jsffi=>({jsffi: {"
    <> mconcat
      ( intersperse
          ","
          [ byteString (entityName k)
              <> ":"
              <> generateFFIImportLambda ffi_decl
            | (k, ffi_decl) <- SM.toList ffiImportDecls
          ]
      )
    <> "}})"
