{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.JSFFI
  ( getFFIModule,
    generateFFIFunctionImports,
    generateFFIImportObjectFactory,
    generateFFIExportObject,
    ffiValueTypeTag,
    ffiValueTypesTag,
  )
where

import Asterius.EDSL
import Asterius.Foreign.Internals
import Asterius.Foreign.SupportedTypes
import Asterius.Passes.GlobalRegs
import Asterius.Types
import qualified CmmCallConv as GHC
import qualified CmmExpr as GHC
import qualified CmmNode as GHC
import Control.Applicative
import Data.Bits
import Data.ByteString.Builder
import Data.Coerce
import Data.IORef
import Data.Int
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)
import qualified Prelude

recoverWasmWrapperValueType :: FFIValueType -> ValueType
recoverWasmWrapperValueType FFIValueType {..} = case ffiValueTypeRep of
  FFILiftedRep -> I64
  FFIUnliftedRep -> I64
  FFIJSValRep -> I64
  FFIIntRep -> I64
  FFIWordRep -> I64
  FFIAddrRep -> I64
  FFIFloatRep -> F32
  FFIDoubleRep -> F64

-- | Get a tag for an FFIValueType
ffiValueTypeTag :: FFIValueType -> Integer
ffiValueTypeTag FFIValueType {ffiValueTypeRep = FFIJSValRep} = 1
ffiValueTypeTag ffi_vt =
  fromIntegral (2 + fromEnum (recoverWasmWrapperValueType ffi_vt))

-- | Get a tag for a list of FFIValueTypes
ffiValueTypesTag :: [FFIValueType] -> Integer
ffiValueTypesTag [] = 0
ffiValueTypesTag [r] = ffiValueTypeTag r
ffiValueTypesTag (r : rs) =
  ffiValueTypeTag r .|. (ffiValueTypesTag rs `shiftL` 3)

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
    param_types = map (const F64) ffiParamTypes
    ret_types = map (const F64) ffiResultTypes

recoverWasmWrapperFunctionType :: FFISafety -> FFIFunctionType -> FunctionType
recoverWasmWrapperFunctionType ffi_safety FFIFunctionType {..}
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

getFFIModule ::
  GHC.DynFlags -> AsteriusModuleSymbol -> Prelude.IO AsteriusModule
getFFIModule dflags mod_sym = do
  ffi_import_state <-
    atomicModifyIORef' globalFFIHookState $ \ffi_hook_state ->
      ( ffi_hook_state
          { ffiHookState = M.delete mod_sym $ ffiHookState ffi_hook_state
          },
        M.findWithDefault mempty mod_sym (ffiHookState ffi_hook_state)
      )
  pure $ generateFFIWrapperModule dflags ffi_import_state

generateImplicitCastExpression ::
  Bool -> [ValueType] -> [ValueType] -> Expression -> Expression
generateImplicitCastExpression signed src_ts dest_ts src_expr =
  case (src_ts, dest_ts) of
    ([I64], [F64]) ->
      Unary
        { unaryOp =
            if signed
              then ConvertSInt64ToFloat64
              else ConvertUInt64ToFloat64,
          operand0 = src_expr
        }
    ([F64], [I64]) ->
      Unary
        { unaryOp = if signed then TruncSFloat64ToInt64 else TruncUFloat64ToInt64,
          operand0 = src_expr
        }
    ([F32], [F64]) -> Unary {unaryOp = PromoteFloat32, operand0 = src_expr}
    ([F64], [F32]) -> Unary {unaryOp = DemoteFloat64, operand0 = src_expr}
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
  GHC.DynFlags -> AsteriusEntitySymbol -> FFIImportDecl -> Function
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
              { target' = coerce k,
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
marshalParamLocation _ = error "Asterius.JSFFI.marshalParamLocation"

recoverCmmType :: GHC.DynFlags -> FFIValueType -> GHC.CmmType
recoverCmmType dflags FFIValueType {..} = case ffiValueTypeRep of
  FFILiftedRep -> GHC.gcWord dflags
  FFIUnliftedRep -> GHC.gcWord dflags
  FFIJSValRep -> GHC.gcWord dflags
  FFIIntRep -> GHC.bWord dflags
  FFIWordRep -> GHC.bWord dflags
  FFIAddrRep -> GHC.bWord dflags
  FFIFloatRep -> GHC.f32
  FFIDoubleRep -> GHC.f64

asyncImportWrapper ::
  GHC.DynFlags -> AsteriusEntitySymbol -> FFIImportDecl -> Function
asyncImportWrapper dflags k FFIImportDecl {..} =
  Function
    { functionType = wrapper_func_type,
      varTypes = [],
      body =
        Block
          { name = "",
            bodys =
              [ CallImport
                  { target' = coerce k,
                    operands =
                      [ generateImplicitCastExpression
                          (ffiValueTypeSigned param_t)
                          [wrapper_param_t]
                          [import_param_t]
                          (unresolvedGetGlobal param_reg)
                        | (param_reg, param_t, wrapper_param_t, import_param_t) <-
                            zip4
                              param_regs
                              ffi_param_types
                              (paramTypes wrapper_func_type)
                              (paramTypes import_func_type)
                      ],
                    callImportReturnTypes = []
                  },
                Store
                  { bytes = 8,
                    offset = fromIntegral $ offset_Capability_r + offset_StgRegTable_rRet,
                    ptr = mainCapability,
                    value = constI64 ret_ThreadBlocked,
                    valueType = I64
                  },
                ReturnCall {returnCallTarget64 = "stg_returnToSchedNotPaused"}
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
        M.fromList
          [ (k <> "_wrapper", wrapper_func)
            | (k, wrapper_func) <- import_wrapper_funcs
          ],
      ffiMarshalState = mod_ffi_state
    }
  where
    import_wrapper_funcs =
      [ (k, generateFFIImportWrapperFunction dflags k ffi_decl)
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
      <> ( case map ffiValueTypeRep ffiResultTypes of
             [FFIJSValRep] -> "__asterius_jsffi.newJSVal("
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
            Field i -> case ffiValueTypeRep $ ffiParamTypes !! (i - 1) of
              FFIJSValRep -> "__asterius_jsffi.getJSVal(_" <> intDec i <> ")"
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

generateFFIExportObject ::
  FFIMarshalState -> M.Map AsteriusEntitySymbol Int64 -> Builder
generateFFIExportObject FFIMarshalState {..} sym_map =
  "{"
    <> mconcat
      ( intersperse
          ","
          [ shortByteString (coerce k)
              <> ":"
              <> generateFFIExportLambda export_decl sym_map
            | (k, export_decl) <- M.toList ffiExportDecls
          ]
      )
    <> "}"

generateFFIExportLambda ::
  FFIExportDecl -> M.Map AsteriusEntitySymbol Int64 -> Builder
generateFFIExportLambda FFIExportDecl {ffiFunctionType = FFIFunctionType {..}, ..} sym_map =
  case (# M.lookup run_func sym_map, M.lookup ffiExportClosure sym_map #) of
    (# Just run_func_addr, Just export_closure_addr #) ->
      let res_func =
            "async function("
              <> mconcat
                ( intersperse
                    ","
                    ["_" <> intDec i | i <- [1 .. length ffiParamTypes]]
                )
              <> "){"
              <> (if null ffiResultTypes then tid else "return " <> ret)
              <> "}"
          ret = case ffiResultTypes of
            [t] ->
              let r = "this.rts_get" <> getHsTyCon t <> "(" <> ret_closure <> ")"
               in case ffiValueTypeRep t of
                    FFIJSValRep ->
                      "this.context.stablePtrManager.getJSVal(" <> r <> ")"
                    _ -> r
            _ -> error "Asterius.JSFFI.generateFFIExportLambda"
          ret_closure = "this.context.scheduler.getTSOret(" <> tid <> ")"
          tid = "await this." <> eval_func <> "(" <> eval_closure <> ")"
          eval_closure =
            "this.rts_apply(0x"
              <> int64HexFixed run_func_addr
              <> ","
              <> foldl'
                ( \acc (i, t) ->
                    "this.rts_apply("
                      <> acc
                      <> ",this.rts_mk"
                      <> getHsTyCon t
                      <> "("
                      <> ( let _i = "_" <> intDec i
                            in case ffiValueTypeRep t of
                                 FFIJSValRep ->
                                   "this.context.stablePtrManager.newJSVal("
                                     <> _i
                                     <> ")"
                                 _ -> _i
                         )
                      <> "))"
                )
                ("0x" <> int64HexFixed export_closure_addr)
                (zip [1 ..] ffiParamTypes)
              <> ")"
       in res_func
    _ -> err_func
  where
    err_func =
      "() => Promise.reject(\""
        <> shortByteString (coerce run_func)
        <> " or "
        <> shortByteString (coerce ffiExportClosure)
        <> " not found\")"
    run_func
      | ffiInIO = "base_AsteriusziTopHandler_runIO_closure"
      | otherwise = "base_AsteriusziTopHandler_runNonIO_closure"
    eval_func
      | null ffiResultTypes = "rts_evalLazyIO"
      | otherwise = "rts_evalIO"
    getHsTyCon FFIValueType {ffiValueTypeRep = FFIJSValRep} = "JSVal"
    getHsTyCon FFIValueType {..} = shortByteString hsTyCon
