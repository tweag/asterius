{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.JSFFI
  ( addFFIProcessor,
    generateFFIFunctionImports,
    generateFFIImportObjectFactory,
    generateFFIExportObject
    )
where

import Asterius.Foreign.Internals
import Asterius.Internals
import Asterius.Types
import Asterius.TypesConv
import Control.Applicative
import Control.Monad.State.Strict
import Data.ByteString.Builder
import qualified Data.ByteString.Short as SBS
import Data.Coerce
import Data.Data
  ( Data,
    gmapM,
    gmapQ
    )
import Data.Functor.Identity
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.String
import qualified ForeignCall as GHC
import qualified GhcPlugins as GHC
import qualified HsSyn as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import qualified PrelNames as GHC
import qualified TcRnTypes as GHC
import Type.Reflection
import qualified TysPrim as GHC
import Prelude hiding (IO)
import qualified Prelude

marshalToFFIValueType
  :: (GHC.HasOccName (GHC.IdP p)) => GHC.LHsType p -> Maybe FFIValueType
marshalToFFIValueType (GHC.unLoc -> GHC.HsParTy _ t) = marshalToFFIValueType t
marshalToFFIValueType (GHC.unLoc -> t) = case t of
  GHC.HsAppTy _ (GHC.unLoc -> (GHC.HsTyVar _ _ (GHC.occName . GHC.unLoc -> c))) _
    | c == GHC.occName GHC.ptrTyConName ->
      pure
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Ptr",
            signed = False
            }
    | c == GHC.occName GHC.funPtrTyConName ->
      pure
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "FunPtr",
            signed = False
            }
    | c
        `elem` map
                 GHC.occName
                 [ GHC.stablePtrTyConName,
                   GHC.getName GHC.stablePtrPrimTyCon,
                   GHC.getName GHC.mutableByteArrayPrimTyCon
                   ] ->
      pure
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "StablePtr",
            signed = False
            }
  GHC.HsTyVar _ _ (GHC.occName . GHC.unLoc -> tv)
    | tv
        `elem` map
                 GHC.occName
                 [GHC.addrPrimTyConName, GHC.getName GHC.byteArrayPrimTyCon] ->
      pure
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Ptr",
            signed = False
            }
    | tv `elem` map GHC.occName [GHC.charTyConName, GHC.charPrimTyConName] ->
      pure
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Char",
            signed = False
            }
    | tv == GHC.occName GHC.boolTyConName ->
      pure
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Bool",
            signed = False
            }
    | tv `elem` map GHC.occName [GHC.intTyConName, GHC.intPrimTyConName] ->
      pure
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Int",
            signed = True
            }
    | tv `elem` map GHC.occName [GHC.wordTyConName, GHC.wordPrimTyConName] ->
      pure
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Word",
            signed = False
            }
    | tv `elem` map GHC.occName [GHC.floatTyConName, GHC.floatPrimTyConName] ->
      pure
        FFI_VAL
          { ffiWasmValueType = F32,
            ffiJSValueType = F32,
            hsTyCon = "Float",
            signed = True
            }
    | tv `elem` map GHC.occName [GHC.doubleTyConName, GHC.doublePrimTyConName] ->
      pure
        FFI_VAL
          { ffiWasmValueType = F64,
            ffiJSValueType = F64,
            hsTyCon = "Double",
            signed = True
            }
    | take 2 (GHC.occNameString tv) == "JS" ->
      pure FFI_JSVAL
  _ -> empty

marshalToFFIResultTypes
  :: (GHC.HasOccName (GHC.IdP p)) => GHC.LHsType p -> Maybe [FFIValueType]
marshalToFFIResultTypes (GHC.unLoc -> GHC.HsParTy _ t) =
  marshalToFFIResultTypes t
marshalToFFIResultTypes (GHC.unLoc -> GHC.HsTupleTy _ _ []) = pure []
marshalToFFIResultTypes t = (: []) <$> marshalToFFIValueType t

marshalToFFIFunctionType
  :: (GHC.HasOccName (GHC.IdP p)) => GHC.LHsType p -> Maybe FFIFunctionType
marshalToFFIFunctionType (GHC.unLoc -> GHC.HsParTy _ t) =
  marshalToFFIFunctionType t
marshalToFFIFunctionType (GHC.unLoc -> ty) = case ty of
  GHC.HsFunTy _ t ts -> do
    vt <- marshalToFFIValueType t
    ft <- marshalToFFIFunctionType ts
    pure ft {ffiParamTypes = vt : ffiParamTypes ft}
  GHC.HsAppTy _ (GHC.unLoc -> (GHC.HsTyVar _ _ (GHC.occName . GHC.unLoc -> io))) t
    | io == GHC.occName GHC.ioTyConName ->
      do
        r <- marshalToFFIResultTypes t
        pure $ FFIFunctionType
          { ffiParamTypes = [],
            ffiResultTypes = r,
            ffiInIO = True
            }
  _ -> do
    r <- marshalToFFIResultTypes (GHC.noLoc ty)
    pure
      FFIFunctionType
        { ffiParamTypes = [],
          ffiResultTypes = r,
          ffiInIO = False
          }

recoverWasmImportValueType :: FFIValueType -> ValueType
recoverWasmImportValueType vt = case vt of
  FFI_VAL {..} -> ffiJSValueType
  FFI_JSVAL -> F64

recoverWasmWrapperValueType :: FFIValueType -> ValueType
recoverWasmWrapperValueType vt = case vt of
  FFI_VAL {..} -> ffiWasmValueType
  FFI_JSVAL -> I64

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
    param_types = map recoverWasmImportValueType ffiParamTypes
    ret_types = map recoverWasmImportValueType ffiResultTypes

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

processFFI :: Data a => a -> State FFIMarshalState a
processFFI = w
  where
    w :: Data a => a -> State FFIMarshalState a
    w t =
      case eqTypeRep (typeOf t) (typeRep :: TypeRep (GHC.ForeignDecl GHC.GhcPs)) of
        Just HRefl -> case t of
          GHC.ForeignExport {GHC.fd_fe = GHC.CExport (GHC.unLoc -> GHC.CExportStatic src_txt lbl GHC.JavaScriptCallConv) loc_src, ..} ->
            do
              old_state@FFIMarshalState {..} <- get
              let Just ffi_ftype =
                    marshalToFFIFunctionType $ GHC.hsImplicitBody fd_sig_ty
              put
                old_state
                  { ffiExportDecls = M.insert
                                       AsteriusEntitySymbol
                                         { entityName = SBS.toShort
                                             $ GHC.fastStringToByteString lbl
                                           }
                                       FFIExportDecl
                                         { ffiFunctionType = ffi_ftype,
                                           ffiExportClosure = ""
                                           }
                                       ffiExportDecls
                    }
              pure
                t
                  { GHC.fd_fe = GHC.CExport
                                  (GHC.noLoc $ GHC.CExportStatic src_txt lbl GHC.CCallConv)
                                  loc_src
                    }
          _ -> pure t
        _ -> gmapM w t

collectFFISrc
  :: Monad m
  => GHC.HsParsedModule
  -> FFIMarshalState
  -> m (GHC.HsParsedModule, FFIMarshalState)
collectFFISrc m ffi_state = pure (m {GHC.hpm_module = new_m}, st)
  where
    (new_m, st) = runState (processFFI (GHC.hpm_module m)) ffi_state

addFFIProcessor
  :: Compiler
  -> IO (Compiler, AsteriusModuleSymbol -> Prelude.IO AsteriusModule)
addFFIProcessor c = do
  ffi_states_ref <- newIORef mempty
  pure
    ( c
        { patchParsed = \mod_summary parsed_mod -> do
            patched_mod <-
              liftIO $ atomicModifyIORef' ffi_states_ref $ \ffi_states ->
                let mod_sym = marshalToModuleSymbol $ GHC.ms_mod mod_summary
                    (patched_mod, ffi_state) =
                      runIdentity $ collectFFISrc parsed_mod mempty
                 in (M.insert mod_sym ffi_state ffi_states, patched_mod)
            patchParsed c mod_summary patched_mod,
          patchTypechecked = \mod_summary tc_mod -> do
            dflags <- GHC.getDynFlags
            let mod_sym = marshalToModuleSymbol $ GHC.ms_mod mod_summary
                f :: Data a => a -> Endo (M.Map AsteriusEntitySymbol FFIExportDecl)
                f t =
                  case eqTypeRep (typeOf t)
                         (typeRep :: TypeRep (GHC.ForeignDecl GHC.GhcTc)) of
                    Just HRefl -> case t of
                      GHC.ForeignExport {..} ->
                        Endo
                          $ M.adjust
                              ( \ffi_decl ->
                                  ffi_decl {ffiExportClosure = export_closure}
                                )
                              export_func_name
                        where
                          GHC.CExport loc_spec _ = fd_fe
                          GHC.CExportStatic _ lbl _ = GHC.unLoc loc_spec
                          export_func_name = AsteriusEntitySymbol
                            { entityName = SBS.toShort
                                $ GHC.fastStringToByteString lbl
                              }
                          export_closure =
                            fromString
                              $ asmPpr dflags (GHC.unLoc fd_name)
                              <> "_closure"
                      _ -> mempty
                    _ -> go
                  where
                    go = mconcat $ gmapQ f t
            liftIO $ atomicModifyIORef' ffi_states_ref $ \ffi_states ->
              ( M.adjust
                  ( \ffi_state ->
                      ffi_state
                        { ffiExportDecls = appEndo (f $ GHC.tcg_fords tc_mod)
                                             (ffiExportDecls ffi_state)
                          }
                    )
                  mod_sym
                  ffi_states,
                tc_mod
                )
          },
      \mod_sym -> do
        ffi_state <-
          atomicModifyIORef' ffi_states_ref
            $ \ffi_states -> (M.delete mod_sym ffi_states, ffi_states ! mod_sym)
        ffi_import_state <-
          atomicModifyIORef' globalFFIHookState $ \ffi_hook_state ->
            ( ffi_hook_state
                { ffiHookState = M.delete mod_sym $ ffiHookState ffi_hook_state
                  },
              M.findWithDefault mempty mod_sym (ffiHookState ffi_hook_state)
              )
        pure $ generateFFIWrapperModule $ ffi_import_state <> ffi_state
      )

generateImplicitCastExpression
  :: Bool -> [ValueType] -> [ValueType] -> Expression -> Expression
generateImplicitCastExpression signed src_ts dest_ts src_expr =
  case (src_ts, dest_ts) of
    ([I64], [F64]) -> Unary
      { unaryOp = if signed
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
        error
          $ "Unsupported implicit cast from "
          <> show src_ts
          <> " to "
          <> show dest_ts

generateFFIImportWrapperFunction
  :: AsteriusEntitySymbol -> FFIImportDecl -> Function
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
          operands = [ generateImplicitCastExpression
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
                           zip4 [0 ..]
                             (ffiParamTypes ffiFunctionType)
                             (paramTypes wrapper_func_type)
                             (paramTypes import_func_type)
                       ],
          callImportReturnTypes = returnTypes import_func_type
          }
    }
  where
    import_func_type = recoverWasmImportFunctionType ffiSafety ffiFunctionType
    wrapper_func_type = recoverWasmWrapperFunctionType ffiSafety ffiFunctionType

generateFFIWrapperModule :: FFIMarshalState -> AsteriusModule
generateFFIWrapperModule mod_ffi_state@FFIMarshalState {..} =
  mempty
    { functionMap = M.fromList
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

generateFFIImportLambda :: FFIImportDecl -> Builder
generateFFIImportLambda FFIImportDecl {ffiFunctionType = FFIFunctionType {..}, ..}
  | is_unsafe =
    lamb
      <> ( case ffiResultTypes of
             [FFI_JSVAL] -> "__asterius_jsffi.newJSVal("
             _ -> "("
           )
      <> code
      <> ")"
  | otherwise =
    lamb
      <> "__asterius_jsffi.setPromise("
      <> intDec
           ( case ffiResultTypes of
               [r] -> succ $ fromEnum $ recoverWasmWrapperValueType r
               _ -> 0
             )
      <> ",Promise.resolve("
      <> code
      <> ")"
      <> ( case ffiResultTypes of
             [FFI_JSVAL] -> ".then(v => __asterius_jsffi.newJSVal(v))"
             _ -> mempty
           )
      <> ")"
  where
    is_unsafe = ffiSafety == FFIUnsafe
    lamb =
      "("
        <> mconcat
             ( intersperse ","
                 ["_" <> intDec i | i <- [1 .. length ffiParamTypes]]
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
    ret_closure = "this.context.tsoManager.getTSOret(" <> tid <> ")"
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
