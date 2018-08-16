{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
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

import Asterius.Internals
import Asterius.Types
import Asterius.TypesConv
import Control.Applicative
import Control.Monad.State.Strict
import Data.Attoparsec.ByteString.Char8
  ( Parser
  , anyChar
  , char
  , decimal
  , parseOnly
  )
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as CBS
import Data.Data (Data, gmapM, gmapQ, gmapT)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.String
import qualified Data.Vector as V
import qualified ForeignCall as GHC
import qualified GhcPlugins as GHC
import qualified HsSyn as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import qualified PrelNames as GHC
import Prelude hiding (IO)
import qualified Prelude
import qualified TcRnTypes as GHC
import Type.Reflection

parseField :: Parser a -> Parser (Chunk a)
parseField f = do
  void $ char '$'
  void $ char '{'
  v <- f
  void $ char '}'
  pure $ Field v

parseChunk :: Parser (Chunk a) -> Parser (Chunk a)
parseChunk f =
  f <|> do
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
parseFFIChunks = combineChunks <$> parseChunks (parseChunk (parseField decimal))

marshalToFFIValueType :: GHC.LHsType GHC.GhcPs -> Maybe FFIValueType
marshalToFFIValueType (GHC.unLoc -> GHC.HsParTy _ t) = marshalToFFIValueType t
marshalToFFIValueType (GHC.unLoc -> t) =
  case t of
    GHC.HsAppTy _ (GHC.unLoc -> (GHC.HsTyVar _ _ (GHC.rdrNameOcc . GHC.unLoc -> c))) _
      | c `elem`
          map
            GHC.nameOccName
            [GHC.ptrTyConName, GHC.funPtrTyConName, GHC.stablePtrTyConName] ->
        pure
          FFI_VAL {ffiWasmValueType = I64, ffiJSValueType = I32, signed = False}
    GHC.HsTyVar _ _ (GHC.rdrNameOcc . GHC.unLoc -> tv)
      | tv == GHC.nameOccName GHC.charTyConName ->
        pure
          FFI_VAL {ffiWasmValueType = I64, ffiJSValueType = I32, signed = False}
      | tv == GHC.nameOccName GHC.intTyConName ->
        pure
          FFI_VAL {ffiWasmValueType = I64, ffiJSValueType = I32, signed = True}
      | tv == GHC.nameOccName GHC.wordTyConName ->
        pure
          FFI_VAL {ffiWasmValueType = I64, ffiJSValueType = I32, signed = False}
      | tv == GHC.nameOccName GHC.floatTyConName ->
        pure
          FFI_VAL {ffiWasmValueType = F32, ffiJSValueType = F32, signed = True}
      | tv == GHC.nameOccName GHC.doubleTyConName ->
        pure
          FFI_VAL {ffiWasmValueType = F64, ffiJSValueType = F64, signed = True}
      | GHC.occNameString tv == "JSRef" -> pure FFI_JSREF
    _ -> empty

marshalToFFIResultType :: GHC.LHsType GHC.GhcPs -> Maybe (Maybe FFIValueType)
marshalToFFIResultType (GHC.unLoc -> GHC.HsParTy _ t) = marshalToFFIResultType t
marshalToFFIResultType (GHC.unLoc -> GHC.HsTupleTy _ _ []) = pure Nothing
marshalToFFIResultType t = Just <$> marshalToFFIValueType t

marshalToFFIFunctionType :: GHC.LHsType GHC.GhcPs -> Maybe FFIFunctionType
marshalToFFIFunctionType (GHC.unLoc -> GHC.HsParTy _ t) =
  marshalToFFIFunctionType t
marshalToFFIFunctionType (GHC.unLoc -> ty) =
  case ty of
    GHC.HsFunTy _ t ts -> do
      vt <- marshalToFFIValueType t
      ft <- marshalToFFIFunctionType ts
      pure ft {ffiParamTypes = vt : ffiParamTypes ft}
    GHC.HsAppTy _ (GHC.unLoc -> (GHC.HsTyVar _ _ (GHC.rdrNameOcc . GHC.unLoc -> io))) t
      | io == GHC.nameOccName GHC.ioTyConName -> do
        r <- marshalToFFIResultType t
        pure $ FFIFunctionType {ffiParamTypes = [], ffiResultType = r}
    _ -> do
      r <- marshalToFFIResultType (GHC.noLoc ty)
      pure FFIFunctionType {ffiParamTypes = [], ffiResultType = r}

rewriteJSRef :: Data a => a -> a
rewriteJSRef t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep GHC.RdrName) of
    Just HRefl ->
      case t of
        GHC.Unqual n
          | GHC.occNameString n == "JSRef" -> GHC.Unqual (GHC.mkTcOcc "Int")
        _ -> t
    _ -> gmapT rewriteJSRef t

recoverWasmImportValueType :: Maybe FFIValueType -> ValueType
recoverWasmImportValueType vt =
  case vt of
    Just FFI_VAL {..} -> ffiJSValueType
    Just FFI_JSREF -> I32
    Nothing -> None

recoverWasmWrapperValueType :: Maybe FFIValueType -> ValueType
recoverWasmWrapperValueType vt =
  case vt of
    Just FFI_VAL {..} -> ffiWasmValueType
    Just FFI_JSREF -> I64
    Nothing -> None

recoverWasmImportFunctionType :: FFIFunctionType -> FunctionType
recoverWasmImportFunctionType FFIFunctionType {..} =
  FunctionType
    { returnType = recoverWasmImportValueType ffiResultType
    , paramTypes =
        V.fromList [recoverWasmImportValueType $ Just t | t <- ffiParamTypes]
    }

recoverWasmWrapperFunctionType :: FFIFunctionType -> FunctionType
recoverWasmWrapperFunctionType FFIFunctionType {..} =
  FunctionType
    { returnType = recoverWasmWrapperValueType ffiResultType
    , paramTypes =
        V.fromList [recoverWasmWrapperValueType $ Just t | t <- ffiParamTypes]
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
            GHC.ForeignImport {fd_fi = GHC.CImport loc_conv _ _ _ loc_src, ..} ->
              case GHC.unLoc loc_conv of
                GHC.JavaScriptCallConv -> do
                  FFIMarshalState {..} <- get
                  let old_decls = ffiImportDecls ! mod_sym
                      new_k = maybe 0 (succ . fst) $ IM.lookupMax old_decls
                      new_decls = IM.insert new_k new_decl old_decls
                  put $
                    FFIMarshalState
                      {ffiImportDecls = HM.singleton mod_sym new_decls}
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
                          marshalToFFIFunctionType $
                          GHC.hsImplicitBody fd_sig_ty
                        Right chunks =
                          parseOnly parseFFIChunks $ CBS.pack $ read src
                        new_decl =
                          FFIImportDecl
                            { ffiFunctionType = ffi_ftype
                            , ffiSourceChunks = chunks
                            }
                _ -> pure t
            GHC.ForeignExport {..} ->
              pure
                t
                  { GHC.fd_fe =
                      GHC.CExport
                        (GHC.noLoc $ GHC.CExportStatic src_txt lbl GHC.CCallConv)
                        loc_src
                  }
              where GHC.CExport loc_spec loc_src = fd_fe
                    GHC.CExportStatic src_txt lbl _ = GHC.unLoc loc_spec
            _ -> pure t
        _ -> gmapM w t

collectFFISrc ::
     AsteriusModuleSymbol
  -> GHC.HsParsedModule
  -> FFIMarshalState
  -> (GHC.HsParsedModule, FFIMarshalState)
collectFFISrc mod_sym m ffi_state =
  (m {GHC.hpm_module = rewriteJSRef new_m}, st)
  where
    (new_m, st) = runState (processFFI mod_sym (GHC.hpm_module m)) ffi_state

addFFIProcessor ::
     Compiler
  -> IO (Compiler, AsteriusModuleSymbol -> Prelude.IO AsteriusModule)
addFFIProcessor c = do
  ffi_mods_ref <- newIORef mempty
  pure
    ( c
        { patchParsed =
            \mod_summary parsed_mod -> do
              patched_mod <-
                liftIO $
                atomicModifyIORef' ffi_mods_ref $ \ffi_mods ->
                  let mod_sym = marshalToModuleSymbol $ GHC.ms_mod mod_summary
                      (patched_mod, ffi_state) =
                        collectFFISrc
                          mod_sym
                          parsed_mod
                          mempty
                            {ffiImportDecls = HM.insert mod_sym mempty mempty}
                      ffi_mod = generateFFIWrapperModule ffi_state
                   in (HM.insert mod_sym ffi_mod ffi_mods, patched_mod)
              patchParsed c mod_summary patched_mod
        , patchTypechecked =
            \_ tc_mod -> do
              dflags <- GHC.getDynFlags
              let f :: Data a => a -> [(String, String)]
                  f t =
                    case eqTypeRep
                           (typeOf t)
                           (typeRep :: TypeRep (GHC.ForeignDecl GHC.GhcTc)) of
                      Just HRefl ->
                        case t of
                          GHC.ForeignExport {..} ->
                            [ ( GHC.unpackFS lbl
                              , asmPpr dflags $ GHC.unLoc fd_name)
                            ]
                            where GHC.CExport loc_spec _ = fd_fe
                                  GHC.CExportStatic _ lbl _ = GHC.unLoc loc_spec
                          _ -> []
                      _ -> go
                    where
                      go = concat $ gmapQ f t
                  ids = f $ GHC.tcg_fords tc_mod
              liftIO $ print ids
              pure tc_mod
        }
    , \mod_sym ->
        atomicModifyIORef' ffi_mods_ref $ \ffi_mods ->
          (HM.delete mod_sym ffi_mods, ffi_mods ! mod_sym))

generateImplicitCastExpression ::
     Bool -> ValueType -> ValueType -> Expression -> Expression
generateImplicitCastExpression signed src_t dest_t src_expr =
  case (src_t, dest_t) of
    (I64, I32) -> Unary {unaryOp = WrapInt64, operand0 = src_expr}
    (I32, I64) ->
      Unary
        { unaryOp =
            if signed
              then ExtendSInt32
              else ExtendUInt32
        , operand0 = src_expr
        }
    _
      | src_t == dest_t -> src_expr
      | otherwise ->
        error $
        "Unsupported implicit cast from " <> show src_t <> " to " <> show dest_t

generateFFIWrapperFunction ::
     AsteriusModuleSymbol -> Int -> FFIImportDecl -> AsteriusFunction
generateFFIWrapperFunction mod_sym k FFIImportDecl {..} =
  AsteriusFunction
    { functionType = recoverWasmWrapperFunctionType ffiFunctionType
    , body =
        generateImplicitCastExpression
          (case ffiResultType ffiFunctionType of
             Just FFI_VAL {..} -> signed
             _ -> False)
          (returnType import_func_type)
          (returnType wrapper_func_type) $
        CallImport
          { target' = fromString $ recoverWasmImportFunctionName mod_sym k
          , operands =
              V.fromList
                [ generateImplicitCastExpression
                  (case param_t of
                     FFI_VAL {..} -> signed
                     _ -> False)
                  wrapper_param_t
                  import_param_t
                  GetLocal {index = i, valueType = wrapper_param_t}
                | (i, param_t, wrapper_param_t, import_param_t) <-
                    zip4
                      [0 ..]
                      (ffiParamTypes ffiFunctionType)
                      (V.toList $ paramTypes wrapper_func_type)
                      (V.toList $ paramTypes import_func_type)
                ]
          , valueType = returnType import_func_type
          }
    }
  where
    import_func_type = recoverWasmImportFunctionType ffiFunctionType
    wrapper_func_type = recoverWasmWrapperFunctionType ffiFunctionType

generateFFIWrapperModule :: FFIMarshalState -> AsteriusModule
generateFFIWrapperModule mod_ffi_state@FFIMarshalState {..} =
  mempty
    { functionMap =
        HM.fromList
          [ (fromString $ recoverWasmWrapperFunctionName mk k, wrapper_func)
          | (mk, k, wrapper_func) <- wrapper_funcs
          ]
    , ffiMarshalState = mod_ffi_state
    }
  where
    wrapper_funcs =
      [ (mk, k, generateFFIWrapperFunction mk k ffi_decl)
      | (mk, mod_ffi_decls) <- HM.toList ffiImportDecls
      , (k, ffi_decl) <- IM.toList mod_ffi_decls
      ]

generateFFIFunctionImports :: FFIMarshalState -> [AsteriusFunctionImport]
generateFFIFunctionImports FFIMarshalState {..} =
  [ AsteriusFunctionImport
    { internalName = fn
    , externalModuleName = "jsffi"
    , externalBaseName = fn
    , functionType = recoverWasmImportFunctionType ffiFunctionType
    }
  | (mk, mod_ffi_decls) <- HM.toList ffiImportDecls
  , (k, FFIImportDecl {..}) <- IM.toList mod_ffi_decls
  , let fn = fromString $ recoverWasmImportFunctionName mk k
  ]

generateFFILambda :: FFIImportDecl -> Builder
generateFFILambda FFIImportDecl {ffiFunctionType = FFIFunctionType {..}, ..} =
  "((" <>
  mconcat (intersperse "," ["_" <> intDec i | i <- [1 .. length ffiParamTypes]]) <>
  ")=>" <>
  (case ffiResultType of
     Just FFI_JSREF -> "ffi.newJSRef("
     _ -> "(") <>
  mconcat
    [ case chunk of
      Lit s -> string7 s
      Field i ->
        case ffiParamTypes !! (i - 1) of
          FFI_JSREF -> "ffi.JSRefs[_" <> intDec i <> "]"
          _ -> "_" <> intDec i
    | chunk <- ffiSourceChunks
    ] <>
  "))"

generateFFIImportObjectFactory :: FFIMarshalState -> Builder
generateFFIImportObjectFactory FFIMarshalState {..} =
  "ffi => ({jsffi: {" <>
  mconcat
    (intersperse
       ","
       [ string7 (recoverWasmImportFunctionName mk k) <> ":" <>
       generateFFILambda ffi_decl
       | (mk, mod_ffi_decls) <- HM.toList ffiImportDecls
       , (k, ffi_decl) <- IM.toList mod_ffi_decls
       ]) <>
  "}})"
