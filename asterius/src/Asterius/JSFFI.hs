{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.JSFFI
  ( addFFIProcessor
  , generateFFIFunctionImports
  , generateFFIImportObjectFactory
  ) where

import Asterius.Builtins
import Asterius.Internals
import Asterius.Types
import Asterius.TypesConv
import Control.Applicative
import Control.Monad.State.Strict
import Data.ByteString.Builder
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapM, gmapQ)
import Data.Functor.Identity
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.String
import qualified ForeignCall as GHC
import qualified GhcPlugins as GHC
import qualified HsSyn as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import qualified PrelNames as GHC
import Prelude hiding (IO)
import qualified Prelude
import qualified TcRnTypes as GHC
import Text.Parsec (anyChar, char, digit, parse, try)
import Text.Parsec.String (Parser)
import Type.Reflection
import qualified TysPrim as GHC

parseField :: Parser a -> Parser (Chunk a)
parseField f = do
  void $ char '$'
  void $ char '{'
  v <- f
  void $ char '}'
  pure $ Field v

parseChunk :: Parser (Chunk a) -> Parser (Chunk a)
parseChunk f =
  try f <|> do
    c <- anyChar
    pure $ Lit [c]

parseChunks :: Parser (Chunk a) -> Parser [Chunk a]
parseChunks = many

combineChunks :: [Chunk a] -> [Chunk a]
combineChunks =
  foldr
    (curry $ \case
       (Lit l, Lit l':cs) -> Lit (l <> l') : cs
       (c, cs) -> c : cs)
    []

parseFFIChunks :: Parser [Chunk Int]
parseFFIChunks =
  combineChunks <$> parseChunks (parseChunk (parseField (read <$> some digit)))

marshalToFFIValueType ::
     (GHC.HasOccName (GHC.IdP p)) => GHC.LHsType p -> Maybe FFIValueType
marshalToFFIValueType (GHC.unLoc -> GHC.HsParTy _ t) = marshalToFFIValueType t
marshalToFFIValueType (GHC.unLoc -> t) =
  case t of
    GHC.HsAppTy _ (GHC.unLoc -> (GHC.HsTyVar _ _ (GHC.occName . GHC.unLoc -> c))) _
      | c == GHC.occName GHC.ptrTyConName ->
        pure
          FFI_VAL
            { ffiWasmValueType = I64
            , ffiJSValueType = F64
            , hsTyCon = "Ptr"
            , signed = False
            }
      | c == GHC.occName GHC.funPtrTyConName ->
        pure
          FFI_VAL
            { ffiWasmValueType = I64
            , ffiJSValueType = F64
            , hsTyCon = "FunPtr"
            , signed = False
            }
      | c `elem`
          map
            GHC.occName
            [GHC.stablePtrTyConName, GHC.getName GHC.stablePtrPrimTyCon] ->
        pure
          FFI_VAL
            { ffiWasmValueType = I64
            , ffiJSValueType = F64
            , hsTyCon = "StablePtr"
            , signed = False
            }
    GHC.HsTyVar _ _ (GHC.occName . GHC.unLoc -> tv)
      | tv == GHC.occName GHC.addrPrimTyConName ->
        pure
          FFI_VAL
            { ffiWasmValueType = I64
            , ffiJSValueType = F64
            , hsTyCon = "Ptr"
            , signed = False
            }
      | tv `elem` map GHC.occName [GHC.charTyConName, GHC.charPrimTyConName] ->
        pure
          FFI_VAL
            { ffiWasmValueType = I64
            , ffiJSValueType = F64
            , hsTyCon = "Char"
            , signed = False
            }
      | tv == GHC.occName GHC.boolTyConName ->
        pure
          FFI_VAL
            { ffiWasmValueType = I64
            , ffiJSValueType = F64
            , hsTyCon = "Bool"
            , signed = False
            }
      | tv `elem` map GHC.occName [GHC.intTyConName, GHC.intPrimTyConName] ->
        pure
          FFI_VAL
            { ffiWasmValueType = I64
            , ffiJSValueType = F64
            , hsTyCon = "Int"
            , signed = True
            }
      | tv `elem` map GHC.occName [GHC.wordTyConName, GHC.wordPrimTyConName] ->
        pure
          FFI_VAL
            { ffiWasmValueType = I64
            , ffiJSValueType = F64
            , hsTyCon = "Word"
            , signed = False
            }
      | tv `elem` map GHC.occName [GHC.floatTyConName, GHC.floatPrimTyConName] ->
        pure
          FFI_VAL
            { ffiWasmValueType = F32
            , ffiJSValueType = F32
            , hsTyCon = "Float"
            , signed = True
            }
      | tv `elem` map GHC.occName [GHC.doubleTyConName, GHC.doublePrimTyConName] ->
        pure
          FFI_VAL
            { ffiWasmValueType = F64
            , ffiJSValueType = F64
            , hsTyCon = "Double"
            , signed = True
            }
      | take 2 (GHC.occNameString tv) == "JS" -> pure FFI_JSREF
    _ -> empty

marshalToFFIResultTypes ::
     (GHC.HasOccName (GHC.IdP p)) => GHC.LHsType p -> Maybe [FFIValueType]
marshalToFFIResultTypes (GHC.unLoc -> GHC.HsParTy _ t) =
  marshalToFFIResultTypes t
marshalToFFIResultTypes (GHC.unLoc -> GHC.HsTupleTy _ _ []) = pure []
marshalToFFIResultTypes t = (: []) <$> marshalToFFIValueType t

marshalToFFIFunctionType ::
     (GHC.HasOccName (GHC.IdP p)) => GHC.LHsType p -> Maybe FFIFunctionType
marshalToFFIFunctionType (GHC.unLoc -> GHC.HsParTy _ t) =
  marshalToFFIFunctionType t
marshalToFFIFunctionType (GHC.unLoc -> ty) =
  case ty of
    GHC.HsFunTy _ t ts -> do
      vt <- marshalToFFIValueType t
      ft <- marshalToFFIFunctionType ts
      pure ft {ffiParamTypes = vt : ffiParamTypes ft}
    GHC.HsAppTy _ (GHC.unLoc -> (GHC.HsTyVar _ _ (GHC.occName . GHC.unLoc -> io))) t
      | io == GHC.occName GHC.ioTyConName -> do
        r <- marshalToFFIResultTypes t
        pure $
          FFIFunctionType
            {ffiParamTypes = [], ffiResultTypes = r, ffiInIO = True}
    _ -> do
      r <- marshalToFFIResultTypes (GHC.noLoc ty)
      pure
        FFIFunctionType
          {ffiParamTypes = [], ffiResultTypes = r, ffiInIO = False}

recoverWasmImportValueType :: FFIValueType -> ValueType
recoverWasmImportValueType vt =
  case vt of
    FFI_VAL {..} -> ffiJSValueType
    FFI_JSREF -> F64

recoverWasmWrapperValueType :: FFIValueType -> ValueType
recoverWasmWrapperValueType vt =
  case vt of
    FFI_VAL {..} -> ffiWasmValueType
    FFI_JSREF -> I64

recoverWasmImportFunctionType :: FFIFunctionType -> FunctionType
recoverWasmImportFunctionType FFIFunctionType {..} =
  FunctionType
    { paramTypes = [recoverWasmImportValueType t | t <- ffiParamTypes]
    , returnTypes = map recoverWasmImportValueType ffiResultTypes
    }

recoverWasmWrapperFunctionType :: FFIFunctionType -> FunctionType
recoverWasmWrapperFunctionType FFIFunctionType {..} =
  FunctionType
    { paramTypes = [recoverWasmWrapperValueType t | t <- ffiParamTypes]
    , returnTypes = map recoverWasmWrapperValueType ffiResultTypes
    }

recoverWasmImportFunctionName :: AsteriusModuleSymbol -> Int -> String
recoverWasmImportFunctionName mod_sym k =
  "__asterius_jsffi_" <> zEncodeModuleSymbol mod_sym <> "_" <> show k

recoverWasmWrapperFunctionName :: AsteriusModuleSymbol -> Int -> String
recoverWasmWrapperFunctionName mod_sym k =
  recoverWasmImportFunctionName mod_sym k <> "_wrapper"

processFFI :: Data a => AsteriusModuleSymbol -> a -> State FFIMarshalState a
processFFI mod_sym = w
  where
    w :: Data a => a -> State FFIMarshalState a
    w t =
      case eqTypeRep (typeOf t) (typeRep :: TypeRep (GHC.ForeignDecl GHC.GhcPs)) of
        Just HRefl ->
          case t of
            GHC.ForeignImport { GHC.fd_fi = GHC.CImport (GHC.unLoc -> GHC.JavaScriptCallConv) _ _ _ loc_src
                              , ..
                              } -> do
              old_state@FFIMarshalState {..} <- get
              let old_decls = ffiImportDecls ! mod_sym
                  new_k = maybe 0 (succ . fst) $ IM.lookupMax old_decls
                  new_decls = IM.insert new_k new_decl old_decls
              put $ old_state {ffiImportDecls = M.singleton mod_sym new_decls}
              pure
                t
                  { GHC.fd_fi =
                      GHC.CImport
                        (GHC.noLoc GHC.CCallConv)
                        (GHC.noLoc GHC.PlayRisky)
                        Nothing
                        (GHC.CFunction $
                         GHC.StaticTarget
                           GHC.NoSourceText
                           (GHC.mkFastString $
                            recoverWasmWrapperFunctionName mod_sym new_k)
                           Nothing
                           True)
                        (GHC.noLoc GHC.NoSourceText)
                  }
              where GHC.SourceText src = GHC.unLoc loc_src
                    Just ffi_ftype =
                      marshalToFFIFunctionType $ GHC.hsImplicitBody fd_sig_ty
                    Right chunks = parse parseFFIChunks src (read src)
                    new_decl =
                      FFIImportDecl
                        {ffiFunctionType = ffi_ftype, ffiSourceChunks = chunks}
            GHC.ForeignExport { GHC.fd_fe = GHC.CExport (GHC.unLoc -> GHC.CExportStatic src_txt lbl GHC.JavaScriptCallConv) loc_src
                              , ..
                              } -> do
              old_state@FFIMarshalState {..} <- get
              let old_decls = ffiExportDecls ! mod_sym
                  Just ffi_ftype =
                    marshalToFFIFunctionType $ GHC.hsImplicitBody fd_sig_ty
                  new_decls =
                    M.insert
                      AsteriusEntitySymbol
                        { entityName =
                            SBS.toShort $ GHC.fastStringToByteString lbl
                        }
                      FFIExportDecl
                        {ffiFunctionType = ffi_ftype, ffiExportClosure = ""}
                      old_decls
              put $ old_state {ffiExportDecls = M.singleton mod_sym new_decls}
              pure
                t
                  { GHC.fd_fe =
                      GHC.CExport
                        (GHC.noLoc $ GHC.CExportStatic src_txt lbl GHC.CCallConv)
                        loc_src
                  }
            _ -> pure t
        _ -> gmapM w t

collectFFISrc ::
     Monad m
  => AsteriusModuleSymbol
  -> GHC.HsParsedModule
  -> FFIMarshalState
  -> m (GHC.HsParsedModule, FFIMarshalState)
collectFFISrc mod_sym m ffi_state = pure (m {GHC.hpm_module = new_m}, st)
  where
    (new_m, st) = runState (processFFI mod_sym (GHC.hpm_module m)) ffi_state

addFFIProcessor ::
     Compiler
  -> IO (Compiler, AsteriusModuleSymbol -> Prelude.IO AsteriusModule)
addFFIProcessor c = do
  ffi_states_ref <- newIORef mempty
  pure
    ( c
        { patchParsed =
            \mod_summary parsed_mod -> do
              patched_mod <-
                liftIO $
                atomicModifyIORef' ffi_states_ref $ \ffi_states ->
                  let mod_sym = marshalToModuleSymbol $ GHC.ms_mod mod_summary
                      (patched_mod, ffi_state) =
                        runIdentity $
                        collectFFISrc
                          mod_sym
                          parsed_mod
                          FFIMarshalState
                            { ffiImportDecls = M.insert mod_sym mempty mempty
                            , ffiExportDecls = M.insert mod_sym mempty mempty
                            }
                   in (M.insert mod_sym ffi_state ffi_states, patched_mod)
              patchParsed c mod_summary patched_mod
        , patchTypechecked =
            \mod_summary tc_mod -> do
              dflags <- GHC.getDynFlags
              let mod_sym = marshalToModuleSymbol $ GHC.ms_mod mod_summary
                  f :: Data a
                    => a
                    -> Endo (M.Map AsteriusEntitySymbol FFIExportDecl)
                  f t =
                    case eqTypeRep
                           (typeOf t)
                           (typeRep :: TypeRep (GHC.ForeignDecl GHC.GhcTc)) of
                      Just HRefl ->
                        case t of
                          GHC.ForeignExport {..} ->
                            Endo $
                            M.adjust
                              (\ffi_decl ->
                                 ffi_decl {ffiExportClosure = export_closure})
                              export_func_name
                            where GHC.CExport loc_spec _ = fd_fe
                                  GHC.CExportStatic _ lbl _ = GHC.unLoc loc_spec
                                  export_func_name =
                                    AsteriusEntitySymbol
                                      { entityName =
                                          SBS.toShort $
                                          GHC.fastStringToByteString lbl
                                      }
                                  export_closure =
                                    fromString $
                                    asmPpr dflags (GHC.unLoc fd_name) <>
                                    "_closure"
                          _ -> mempty
                      _ -> go
                    where
                      go = mconcat $ gmapQ f t
              liftIO $
                atomicModifyIORef' ffi_states_ref $ \ffi_state ->
                  ( M.adjust
                      (\s ->
                         s
                           { ffiExportDecls =
                               M.adjust
                                 (appEndo $ f $ GHC.tcg_fords tc_mod)
                                 mod_sym $
                               ffiExportDecls s
                           })
                      mod_sym
                      ffi_state
                  , tc_mod)
        }
    , \mod_sym ->
        atomicModifyIORef' ffi_states_ref $ \ffi_states ->
          ( M.delete mod_sym ffi_states
          , generateFFIWrapperModule $ ffi_states ! mod_sym))

generateImplicitCastExpression ::
     Bool -> [ValueType] -> [ValueType] -> Expression -> Expression
generateImplicitCastExpression signed src_ts dest_ts src_expr =
  case (src_ts, dest_ts) of
    ([I64], [F64]) ->
      Unary
        { unaryOp =
            if signed
              then ConvertSInt64ToFloat64
              else ConvertUInt64ToFloat64
        , operand0 = src_expr
        }
    ([F64], [I64]) ->
      Unary
        { unaryOp =
            if signed
              then TruncSFloat64ToInt64
              else TruncUFloat64ToInt64
        , operand0 = src_expr
        }
    _
      | src_ts == dest_ts -> src_expr
      | otherwise ->
        error $
        "Unsupported implicit cast from " <> show src_ts <> " to " <>
        show dest_ts

generateFFIImportWrapperFunction ::
     AsteriusModuleSymbol -> Int -> FFIImportDecl -> AsteriusFunction
generateFFIImportWrapperFunction mod_sym k FFIImportDecl {..} =
  AsteriusFunction
    { functionType = recoverWasmWrapperFunctionType ffiFunctionType
    , body =
        generateImplicitCastExpression
          (case ffiResultTypes ffiFunctionType of
             [FFI_VAL {..}] -> signed
             _ -> False)
          (returnTypes import_func_type)
          (returnTypes wrapper_func_type) $
        CallImport
          { target' = fromString $ recoverWasmImportFunctionName mod_sym k
          , operands =
              [ generateImplicitCastExpression
                (case param_t of
                   FFI_VAL {..} -> signed
                   _ -> False)
                [wrapper_param_t]
                [import_param_t]
                GetLocal {index = i, valueType = wrapper_param_t}
              | (i, param_t, wrapper_param_t, import_param_t) <-
                  zip4
                    [0 ..]
                    (ffiParamTypes ffiFunctionType)
                    (paramTypes wrapper_func_type)
                    (paramTypes import_func_type)
              ]
          , callImportReturnTypes = returnTypes import_func_type
          }
    }
  where
    import_func_type = recoverWasmImportFunctionType ffiFunctionType
    wrapper_func_type = recoverWasmWrapperFunctionType ffiFunctionType

generateFFIExportFunction :: FFIExportDecl -> AsteriusFunction
generateFFIExportFunction FFIExportDecl {..} =
  AsteriusFunction
    { functionType = recoverWasmWrapperFunctionType ffiFunctionType
    , body =
        Block
          { name = ""
          , bodys =
              [ UnresolvedSetLocal
                  { unresolvedLocalReg = tid
                  , value =
                      Call
                        { target =
                            if ffiInIO ffiFunctionType
                              then "rts_evalIO"
                              else "rts_eval"
                        , operands =
                            [ foldl'
                                (\tot_expr (ffi_param_i, ffi_param_t) ->
                                   Call
                                     { target = "rts_apply"
                                     , operands =
                                         [ tot_expr
                                         , Call
                                             { target =
                                                 AsteriusEntitySymbol
                                                   { entityName =
                                                       "rts_mk" <>
                                                       (case ffi_param_t of
                                                          FFI_VAL {..} ->
                                                            hsTyCon
                                                          FFI_JSREF -> "Int")
                                                   }
                                             , operands =
                                                 [ GetLocal
                                                     { index = ffi_param_i
                                                     , valueType =
                                                         recoverWasmWrapperValueType
                                                           ffi_param_t
                                                     }
                                                 ]
                                             , callReturnTypes = [I64]
                                             }
                                         ]
                                     , callReturnTypes = [I64]
                                     })
                                Symbol
                                  { unresolvedSymbol = ffiExportClosure
                                  , symbolOffset = 0
                                  , resolvedSymbol = Nothing
                                  }
                                (zip [0 ..] $ ffiParamTypes ffiFunctionType)
                            ]
                        , callReturnTypes = [I32]
                        }
                  }
              , Call
                  { target = "rts_checkSchedStatus"
                  , operands = [UnresolvedGetLocal {unresolvedLocalReg = tid}]
                  , callReturnTypes = []
                  }
              ] <>
              case ffiResultTypes ffiFunctionType of
                [ffi_result_t] ->
                  [ Call
                      { target =
                          AsteriusEntitySymbol
                            {entityName = "rts_get" <> hsTyCon ffi_result_t}
                      , operands =
                          [ Unary TruncUFloat64ToInt64 $
                            CallImport
                              { target' = "__asterius_getTSOret"
                              , operands =
                                  [ UnresolvedGetLocal
                                      {unresolvedLocalReg = tid}
                                  ]
                              , callImportReturnTypes = [F64]
                              }
                          ]
                      , callReturnTypes =
                          [recoverWasmWrapperValueType ffi_result_t]
                      }
                  ]
                _ -> []
          , blockReturnTypes =
              map recoverWasmWrapperValueType $ ffiResultTypes ffiFunctionType
          }
    }
  where
    tid = UniqueLocalReg 0 I32

generateFFIWrapperModule :: FFIMarshalState -> AsteriusModule
generateFFIWrapperModule mod_ffi_state@FFIMarshalState {..} =
  mempty
    { functionMap =
        M.fromList $
        [ (fromString $ recoverWasmWrapperFunctionName mk k, wrapper_func)
        | (mk, k, wrapper_func) <- import_wrapper_funcs
        ] <>
        export_funcs <>
        export_wrapper_funcs
    , ffiMarshalState = mod_ffi_state
    }
  where
    import_wrapper_funcs =
      [ (mk, k, generateFFIImportWrapperFunction mk k ffi_decl)
      | (mk, mod_ffi_decls) <- M.toList ffiImportDecls
      , (k, ffi_decl) <- IM.toList mod_ffi_decls
      ]
    export_funcs =
      [ (k, generateFFIExportFunction ffi_decl)
      | mod_ffi_decls <- M.elems ffiExportDecls
      , (k, ffi_decl) <- M.toList mod_ffi_decls
      ]
    export_wrapper_funcs =
      [ ( AsteriusEntitySymbol
            {entityName = "__asterius_jsffi_export_" <> entityName k}
        , generateWrapperFunction k f)
      | (k, f) <- export_funcs
      ]

generateFFIFunctionImports :: FFIMarshalState -> [FunctionImport]
generateFFIFunctionImports FFIMarshalState {..} =
  [ FunctionImport
    { internalName = fn
    , externalModuleName = "jsffi"
    , externalBaseName = fn
    , functionType = recoverWasmImportFunctionType ffiFunctionType
    }
  | (mk, mod_ffi_decls) <- M.toList ffiImportDecls
  , (k, FFIImportDecl {..}) <- IM.toList mod_ffi_decls
  , let fn = fromString $ recoverWasmImportFunctionName mk k
  ]

generateFFILambda :: FFIImportDecl -> Builder
generateFFILambda FFIImportDecl {ffiFunctionType = FFIFunctionType {..}, ..} =
  "((" <>
  mconcat (intersperse "," ["_" <> intDec i | i <- [1 .. length ffiParamTypes]]) <>
  ")=>" <>
  (case ffiResultTypes of
     [FFI_JSREF] -> "__asterius_jsffi.newJSRef("
     _ -> "(") <>
  mconcat
    [ case chunk of
      Lit s -> string7 s
      Field i ->
        case ffiParamTypes !! (i - 1) of
          FFI_JSREF -> "__asterius_jsffi.JSRefs[_" <> intDec i <> "]"
          _ -> "_" <> intDec i
    | chunk <- ffiSourceChunks
    ] <>
  "))"

generateFFIImportObjectFactory :: FFIMarshalState -> Builder
generateFFIImportObjectFactory FFIMarshalState {..} =
  "__asterius_jsffi => ({jsffi: {" <>
  mconcat
    (intersperse
       ","
       [ string7 (recoverWasmImportFunctionName mk k) <> ":" <>
       generateFFILambda ffi_decl
       | (mk, mod_ffi_decls) <- M.toList ffiImportDecls
       , (k, ffi_decl) <- IM.toList mod_ffi_decls
       ]) <>
  "}})"
