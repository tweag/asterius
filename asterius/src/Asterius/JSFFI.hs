{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.JSFFI
  ( getFFIModule,
    generateFFIFunctionImports,
    generateFFIImportObjectFactory,
    generateFFIExportObject,
    ffiValueTypeTag,
    ffiValueTypesTag,
  )
where

import Asterius.Foreign.Internals
import Asterius.Types
import Control.Applicative
import Data.Bits
import Data.ByteString.Builder
import Data.Coerce
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Prelude hiding (IO)
import qualified Prelude

recoverWasmImportValueType :: FFIValueType -> ValueType
recoverWasmImportValueType vt = case vt of
  FFI_VAL {..} -> ffiJSValueType
  FFI_JSVAL -> F64

recoverWasmWrapperValueType :: FFIValueType -> ValueType
recoverWasmWrapperValueType vt = case vt of
  FFI_VAL {..} -> ffiWasmValueType
  FFI_JSVAL -> I64

-- | Get a tag for an FFIValueType
ffiValueTypeTag :: FFIValueType -> Integer
ffiValueTypeTag FFI_JSVAL = 1
ffiValueTypeTag FFI_VAL {..} = fromIntegral (2 + fromEnum ffiWasmValueType)

-- | Get a tag for a list of FFIValueTypes
ffiValueTypesTag :: [FFIValueType] -> Integer
ffiValueTypesTag [] = 0
ffiValueTypesTag [r] = ffiValueTypeTag r
ffiValueTypesTag (r : rs) =
  ffiValueTypeTag r .|. (ffiValueTypesTag rs `shiftL` 3)

recoverWasmImportFunctionType :: FFISafety -> FFIFunctionType -> FunctionType
recoverWasmImportFunctionType ffi_safety FFIFunctionType {..}
  | is_unsafe = FunctionType
    { paramTypes = param_types,
      returnTypes = ret_types
    }
  | otherwise = FunctionType
    { paramTypes = I32 : param_types,
      returnTypes = []
    }
  where
    is_unsafe = ffi_safety == FFIUnsafe
    param_types = map recoverWasmImportValueType ffiParamTypes
    ret_types = map recoverWasmImportValueType ffiResultTypes

recoverWasmWrapperFunctionType :: FFISafety -> FFIFunctionType -> FunctionType
recoverWasmWrapperFunctionType ffi_safety FFIFunctionType {..}
  | is_unsafe = FunctionType
    { paramTypes = param_types,
      returnTypes = ret_types
    }
  | otherwise = FunctionType
    { paramTypes = I32 : param_types,
      returnTypes = []
    }
  where
    is_unsafe = ffi_safety == FFIUnsafe
    param_types = map recoverWasmWrapperValueType ffiParamTypes
    ret_types = map recoverWasmWrapperValueType ffiResultTypes

getFFIModule :: AsteriusModuleSymbol -> Prelude.IO AsteriusModule
getFFIModule mod_sym = do
  ffi_import_state <-
    atomicModifyIORef' globalFFIHookState $ \ffi_hook_state ->
      ( ffi_hook_state
          { ffiHookState = M.delete mod_sym $ ffiHookState ffi_hook_state
          },
        M.findWithDefault mempty mod_sym (ffiHookState ffi_hook_state)
      )
  pure $ generateFFIWrapperModule ffi_import_state

generateImplicitCastExpression ::
  Bool -> [ValueType] -> [ValueType] -> Expression -> Expression
generateImplicitCastExpression signed src_ts dest_ts src_expr =
  case (src_ts, dest_ts) of
    ([I64], [F64]) -> Unary
      { unaryOp =
          if signed
            then ConvertSInt64ToFloat64
            else ConvertUInt64ToFloat64,
        operand0 = src_expr
      }
    ([F64], [I64]) -> Unary
      { unaryOp = if signed then TruncSFloat64ToInt64 else TruncUFloat64ToInt64,
        operand0 = src_expr
      }
    _
      | src_ts == dest_ts ->
        src_expr
      | otherwise ->
        error $
          "Unsupported implicit cast from "
            <> show src_ts
            <> " to "
            <> show dest_ts

generateFFIImportWrapperFunction ::
  AsteriusEntitySymbol -> FFIImportDecl -> Function
generateFFIImportWrapperFunction k FFIImportDecl {..} = Function
  { functionType = wrapper_func_type,
    varTypes = [],
    body = generateImplicitCastExpression
      ( case ffiResultTypes ffiFunctionType of
          [FFI_VAL {..}] -> signed
          _ -> False
      )
      (returnTypes import_func_type)
      (returnTypes wrapper_func_type)
      $ CallImport
        { target' = coerce k,
          operands =
            [ generateImplicitCastExpression
                ( case param_t of
                    FFI_VAL {..} -> signed
                    _ -> False
                )
                [wrapper_param_t]
                [import_param_t]
                GetLocal
                  { index = i,
                    valueType = wrapper_param_t
                  }
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
  where
    is_unsafe = ffiSafety == FFIUnsafe
    ffi_param_types
      | is_unsafe =
        ffiParamTypes ffiFunctionType
      | otherwise =
        FFI_VAL
          { ffiWasmValueType = I32,
            ffiJSValueType = I32,
            hsTyCon = "",
            signed = False
          }
          : ffiParamTypes ffiFunctionType
    import_func_type = recoverWasmImportFunctionType ffiSafety ffiFunctionType
    wrapper_func_type = recoverWasmWrapperFunctionType ffiSafety ffiFunctionType

generateFFIWrapperModule :: FFIMarshalState -> AsteriusModule
generateFFIWrapperModule mod_ffi_state@FFIMarshalState {..} =
  mempty
    { functionMap =
        M.fromList
          [ (k <> "_wrapper", wrapper_func)
            | (k, wrapper_func) <- import_wrapper_funcs
          ],
      ffiMarshalState = mod_ffi_state
    }
  where
    import_wrapper_funcs =
      [ (k, generateFFIImportWrapperFunction k ffi_decl)
        | (k, ffi_decl) <- M.toList ffiImportDecls
      ]

generateFFIFunctionImports :: FFIMarshalState -> [FunctionImport]
generateFFIFunctionImports FFIMarshalState {..} =
  [ FunctionImport
      { internalName = coerce k,
        externalModuleName = "jsffi",
        externalBaseName = coerce k,
        functionType = recoverWasmImportFunctionType ffiSafety ffiFunctionType
      }
    | (k, FFIImportDecl {..}) <- M.toList ffiImportDecls
  ]

-- | Generate FFI import lambda
--
-- Unsafe:
--    * Return a JSVal: (_1,_2,...,_N) => __asterius_jsffi.newJSVal(code)
--    * Otherwise:      (_1,_2,...,_N) => (code)
-- Safe:
--    * (tid,_1,_2,...,_N) => __asterius_jsffi.returnFFIPromise(tid, (async () => (code))().then(v => [returnTypes,v]))
--
-- Note: we use `(async () => (code))()` instead of `Promise.resolve(code)` to
-- handle `code` throwing synchronously instead of returning a rejected Promise.
generateFFIImportLambda :: FFIImportDecl -> Builder
generateFFIImportLambda FFIImportDecl {ffiFunctionType = FFIFunctionType {..}, ..}
  | is_unsafe =
    lamb False
      <> ( case ffiResultTypes of
             [FFI_JSVAL] -> "__asterius_jsffi.newJSVal("
             _ -> "("
         )
      <> code
      <> ")"
  | otherwise =
    lamb True
      <> "__asterius_jsffi.returnFFIPromise(tid,"
      <> "(async () => ("
      <> code
      <> "))().then(v => ["
      <> integerDec (ffiValueTypesTag ffiResultTypes)
      <> ",v]))"
  where
    is_unsafe = ffiSafety == FFIUnsafe
    lamb hasTID =
      "("
        <> mconcat
          ( intersperse
              ","
              ( (if hasTID then ["tid"] else [])
                  ++ ["_" <> intDec i | i <- [1 .. length ffiParamTypes]]
              )
          )
        <> ")=>"
    code =
      mconcat
        [ case chunk of
            Lit s -> stringUtf8 s
            Field i -> case ffiParamTypes !! (i - 1) of
              FFI_JSVAL -> "__asterius_jsffi.getJSVal(_" <> intDec i <> ")"
              _ -> "_" <> intDec i
          | chunk <- ffiSourceChunks
        ]

generateFFIImportObjectFactory :: FFIMarshalState -> Builder
generateFFIImportObjectFactory FFIMarshalState {..} =
  "__asterius_jsffi=>({jsffi: {"
    <> mconcat
      ( intersperse
          ","
          [ shortByteString (coerce k)
              <> ":"
              <> generateFFIImportLambda ffi_decl
            | (k, ffi_decl) <- M.toList ffiImportDecls
          ]
      )
    <> "}})"

generateFFIExportObject :: FFIMarshalState -> Builder
generateFFIExportObject FFIMarshalState {..} =
  "{"
    <> mconcat
      ( intersperse
          ","
          [ shortByteString (coerce k)
              <> ":"
              <> generateFFIExportLambda export_decl
            | (k, export_decl) <- M.toList ffiExportDecls
          ]
      )
    <> "}"

generateFFIExportLambda :: FFIExportDecl -> Builder
generateFFIExportLambda FFIExportDecl {ffiFunctionType = FFIFunctionType {..}, ..} =
  "async function("
    <> mconcat
      ( intersperse "," ["_" <> intDec i | i <- [1 .. length ffiParamTypes]]
      )
    <> "){"
    <> (if null ffiResultTypes then tid else "return " <> ret)
    <> "}"
  where
    ret = case ffiResultTypes of
      [t] ->
        let r = "this.rts_get" <> getHsTyCon t <> "(" <> ret_closure <> ")"
         in case t of
              FFI_JSVAL -> "this.context.stablePtrManager.getJSVal(" <> r <> ")"
              _ -> r
      _ -> error "Asterius.JSFFI.generateFFIExportLambda"
    ret_closure = "this.context.scheduler.getTSOret(" <> tid <> ")"
    tid = "await this." <> eval_func <> "(" <> eval_closure <> ")"
    eval_func
      | ffiInIO = "rts_evalIO"
      | otherwise = "rts_eval"
    eval_closure =
      foldl'
        ( \acc (i, t) ->
            "this.rts_apply("
              <> acc
              <> ",this.rts_mk"
              <> getHsTyCon t
              <> "("
              <> ( let _i = "_" <> intDec i
                    in case t of
                         FFI_JSVAL ->
                           "this.context.stablePtrManager.newJSVal(" <> _i <> ")"
                         _ -> _i
                 )
              <> "))"
        )
        ("this.context.symbolTable." <> shortByteString (coerce ffiExportClosure))
        (zip [1 ..] ffiParamTypes)
    getHsTyCon FFI_VAL {..} = shortByteString hsTyCon
    getHsTyCon FFI_JSVAL = "StablePtr"
