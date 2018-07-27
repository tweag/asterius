{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.JSFFI
  ( emptyFFIMarshalState
  , addFFIProcessor
  , generateFFIFunctionImports
  , generateFFIDict
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
import Data.Data (Data, gmapM, gmapT)
import Data.Functor
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
import Prelude hiding (IO)
import qualified Prelude
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

emptyFFIMarshalState :: FFIMarshalState
emptyFFIMarshalState = FFIMarshalState {ffiDecls = mempty}

marshalToFFIValueType :: GHC.Located GHC.RdrName -> Maybe FFIValueType
marshalToFFIValueType loc_t =
  case GHC.occNameString $ GHC.rdrNameOcc $ GHC.unLoc loc_t of
    "Char" ->
      Just
        FFI_VAL
          { ffiWasmValueType = I64
          , ffiJSValueType = F64
          , hsValueTypeModule = "GHC.Types"
          , hsValueTypeName = "Char"
          , signed = False
          }
    "Int" ->
      Just
        FFI_VAL
          { ffiWasmValueType = I64
          , ffiJSValueType = F64
          , hsValueTypeModule = "GHC.Types"
          , hsValueTypeName = "Int"
          , signed = True
          }
    "Word" ->
      Just
        FFI_VAL
          { ffiWasmValueType = I64
          , ffiJSValueType = F64
          , hsValueTypeModule = "GHC.Types"
          , hsValueTypeName = "Word"
          , signed = False
          }
    "Float" ->
      Just
        FFI_VAL
          { ffiWasmValueType = F32
          , ffiJSValueType = F32
          , hsValueTypeModule = "GHC.Types"
          , hsValueTypeName = "Float"
          , signed = True
          }
    "Double" ->
      Just
        FFI_VAL
          { ffiWasmValueType = F64
          , ffiJSValueType = F64
          , hsValueTypeModule = "GHC.Types"
          , hsValueTypeName = "Double"
          , signed = True
          }
    "JSRef" -> Just FFI_JSREF
    _ -> Nothing

marshalToFFIFunctionType :: GHC.LHsType GHC.GhcPs -> Maybe FFIFunctionType
marshalToFFIFunctionType loc_ty =
  case GHC.unLoc loc_ty of
    GHC.HsTyVar _ _ loc_t -> do
      r <- marshalToFFIValueType loc_t
      pure
        FFIFunctionType
          {ffiParamTypes = [], ffiResultType = Just r, ffiInIO = False}
    GHC.HsTupleTy _ _ [] ->
      pure
        FFIFunctionType
          {ffiParamTypes = [], ffiResultType = Nothing, ffiInIO = False}
    GHC.HsAppTy _ c t ->
      case GHC.unLoc c of
        GHC.HsTyVar _ _ loc_io
          | GHC.occNameString (GHC.rdrNameOcc $ GHC.unLoc loc_io) == "IO" -> do
            r <-
              case GHC.unLoc t of
                GHC.HsTyVar _ _ loc_t -> Just <$> marshalToFFIValueType loc_t
                GHC.HsTupleTy _ _ [] -> Just Nothing
                _ -> Nothing
            pure
              FFIFunctionType
                {ffiParamTypes = [], ffiResultType = r, ffiInIO = True}
        _ -> Nothing
    GHC.HsFunTy _ t ts -> do
      vt <-
        case GHC.unLoc t of
          GHC.HsTyVar _ _ loc_t -> marshalToFFIValueType loc_t
          _ -> Nothing
      ft <- marshalToFFIFunctionType ts
      pure ft {ffiParamTypes = vt : ffiParamTypes ft}
    _ -> Nothing

rewriteJSRef :: Data a => a -> a
rewriteJSRef t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep GHC.RdrName) of
    Just HRefl ->
      case t of
        GHC.Unqual n
          | GHC.occNameString n == "JSRef" ->
            GHC.Qual (GHC.mkModuleName "GHC.Types") (GHC.mkTcOcc "Double")
        _ -> t
    _ -> gmapT rewriteJSRef t

mkQualTyCon :: String -> String -> GHC.LHsType GHC.GhcPs
mkQualTyCon m t =
  GHC.noLoc $
  GHC.HsTyVar GHC.NoExt GHC.NotPromoted $
  GHC.noLoc $ GHC.Qual (GHC.mkModuleName m) (GHC.mkTcOcc t)

recoverHsType :: Maybe FFIValueType -> GHC.LHsType GHC.GhcPs
recoverHsType t =
  case t of
    Just FFI_VAL {..} -> mkQualTyCon hsValueTypeModule hsValueTypeName
    Just FFI_JSREF -> mkQualTyCon "GHC.Types" "Double"
    Nothing -> mkQualTyCon "GHC.Tuple" "()"

recoverHsFunctionType :: FFIFunctionType -> GHC.LHsType GHC.GhcPs
recoverHsFunctionType FFIFunctionType {..} =
  foldr
    (\vt hs_t ->
       GHC.noLoc $ GHC.HsFunTy GHC.NoExt (recoverHsType $ Just vt) hs_t)
    (let hs_t = recoverHsType ffiResultType
      in if ffiInIO
           then GHC.noLoc $
                GHC.HsAppTy GHC.NoExt (mkQualTyCon "GHC.Types" "IO") hs_t
           else hs_t)
    ffiParamTypes

recoverWasmImportValueType :: Maybe FFIValueType -> ValueType
recoverWasmImportValueType vt =
  case vt of
    Just FFI_VAL {..} -> ffiJSValueType
    Just FFI_JSREF -> F64
    Nothing -> None

recoverWasmWrapperValueType :: Maybe FFIValueType -> ValueType
recoverWasmWrapperValueType vt =
  case vt of
    Just FFI_VAL {..} -> ffiWasmValueType
    Just FFI_JSREF -> F64
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
                  let old_decls = ffiDecls ! mod_sym
                      new_k = maybe 0 (succ . fst) $ IM.lookupMax old_decls
                      new_decls = IM.insert new_k new_decl old_decls
                  put $
                    FFIMarshalState {ffiDecls = HM.singleton mod_sym new_decls}
                  pure
                    t
                      { GHC.fd_sig_ty =
                          GHC.mkHsImplicitBndrs $
                          recoverHsFunctionType ffi_ftype
                      , GHC.fd_fi =
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
                          FFIDecl
                            { ffiFunctionType = ffi_ftype
                            , ffiSourceChunks = chunks
                            }
                _ -> pure t
            _ -> pure t
        _ -> gmapM w t

collectFFISrc ::
     AsteriusModuleSymbol
  -> GHC.HsParsedModule
  -> FFIMarshalState
  -> (GHC.HsParsedModule, FFIMarshalState)
collectFFISrc mod_sym m ffi_state =
  (m {GHC.hpm_module = rewriteJSRef imp_m}, st)
  where
    (new_m, st) = runState (processFFI mod_sym (GHC.hpm_module m)) ffi_state
    imp_m
      | IM.null (ffiDecls st ! mod_sym) = new_m
      | otherwise =
        new_m $>
        new_m_unloc
          { GHC.hsmodImports =
              [ GHC.noLoc
                GHC.ImportDecl
                  { GHC.ideclExt = GHC.NoExt
                  , GHC.ideclSourceSrc = GHC.NoSourceText
                  , GHC.ideclName = GHC.noLoc $ GHC.mkModuleName imp
                  , GHC.ideclPkgQual = Nothing
                  , GHC.ideclSource = False
                  , GHC.ideclSafe = False
                  , GHC.ideclQualified = True
                  , GHC.ideclImplicit = False
                  , GHC.ideclAs = Nothing
                  , GHC.ideclHiding = Nothing
                  }
              | imp <- ["GHC.Tuple", "GHC.Types"]
              ] <>
              GHC.hsmodImports new_m_unloc
          }
      where
        new_m_unloc = GHC.unLoc new_m

addFFIProcessor ::
     Compiler
  -> IO (Compiler, AsteriusModuleSymbol -> Prelude.IO AsteriusModule)
addFFIProcessor c = do
  ffi_mods_ref <- newIORef mempty
  pure
    ( c
        { patch =
            \mod_summary parsed_mod -> do
              patched_mod <-
                liftIO $
                atomicModifyIORef' ffi_mods_ref $ \ffi_mods ->
                  let mod_sym = marshalToModuleSymbol $ GHC.ms_mod mod_summary
                      (patched_mod, ffi_state) =
                        collectFFISrc
                          mod_sym
                          parsed_mod
                          mempty {ffiDecls = HM.insert mod_sym mempty mempty}
                      ffi_mod = generateFFIWrapperModule ffi_state
                   in (HM.insert mod_sym ffi_mod ffi_mods, patched_mod)
              patch c mod_summary patched_mod
        }
    , \mod_sym ->
        atomicModifyIORef' ffi_mods_ref $ \ffi_mods ->
          (HM.delete mod_sym ffi_mods, ffi_mods ! mod_sym))

generateImplicitCastExpression ::
     Bool -> ValueType -> ValueType -> Expression -> Expression
generateImplicitCastExpression signed src_t dest_t src_expr =
  case (src_t, dest_t) of
    (I64, F64) ->
      Unary
        { unaryOp =
            if signed
              then ConvertSInt64ToFloat64
              else ConvertUInt64ToFloat64
        , operand0 = src_expr
        }
    (F64, I64) ->
      Unary
        { unaryOp =
            if signed
              then TruncSFloat64ToInt64
              else TruncUFloat64ToInt64
        , operand0 = src_expr
        }
    _
      | src_t == dest_t -> src_expr
      | otherwise ->
        error $
        "Unsupported implicit cast from " <> show src_t <> " to " <> show dest_t

generateFFIWrapperFunction ::
     AsteriusModuleSymbol -> Int -> FFIDecl -> AsteriusFunction
generateFFIWrapperFunction mod_sym k FFIDecl {..} =
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
      | (mk, mod_ffi_decls) <- HM.toList ffiDecls
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
  | (mk, mod_ffi_decls) <- HM.toList ffiDecls
  , (k, FFIDecl {..}) <- IM.toList mod_ffi_decls
  , let fn = fromString $ recoverWasmImportFunctionName mk k
  ]

generateFFILambda :: FFIDecl -> Builder
generateFFILambda FFIDecl {ffiFunctionType = FFIFunctionType {..}, ..} =
  "((" <>
  mconcat (intersperse "," ["_" <> intDec i | i <- [1 .. length ffiParamTypes]]) <>
  ")=>" <>
  (case ffiResultType of
     Just FFI_JSREF -> "__asterius_jsffi_newJSRef("
     _ -> "(") <>
  mconcat
    [ case chunk of
      Lit s -> string7 s
      Field i ->
        case ffiParamTypes !! (i - 1) of
          FFI_JSREF -> "__asterius_jsffi_JSRefs[_" <> intDec i <> "]"
          _ -> "_" <> intDec i
    | chunk <- ffiSourceChunks
    ] <>
  "))"

generateFFIDict :: FFIMarshalState -> Builder
generateFFIDict FFIMarshalState {..} =
  "{" <>
  mconcat
    (intersperse
       ","
       [ string7 (recoverWasmImportFunctionName mk k) <> ":" <>
       generateFFILambda ffi_decl
       | (mk, mod_ffi_decls) <- HM.toList ffiDecls
       , (k, ffi_decl) <- IM.toList mod_ffi_decls
       ]) <>
  "}"
