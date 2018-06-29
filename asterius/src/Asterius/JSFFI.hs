{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.JSFFI
  ( Chunk(..)
  , FFIValueType(..)
  , FFIFunctionType(..)
  , FFIDecl(..)
  , FFIMarshalState(..)
  , emptyFFIMarshalState
  , addJSFFIProcessor
  , generateFFIFunctionTypeMap
  , generateFFIFunctionImports
  , generateJSFFIDict
  ) where

import Asterius.Types
import Control.Applicative
import Control.Monad.State.Strict
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Short as SBS
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
import Type.Reflection

data Chunk a
  = Lit String
  | Field a
  deriving (Show)

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

data FFIValueType
  = FFI_F32
  | FFI_F64
  | FFI_REF
  deriving (Show)

data FFIFunctionType = FFIFunctionType
  { ffiParamTypes :: [FFIValueType]
  , ffiResultType :: Maybe FFIValueType
  , ffiInIO :: Bool
  } deriving (Show)

data FFIDecl = FFIDecl
  { ffiFunctionType :: FFIFunctionType
  , ffiSourceChunks :: [Chunk Int]
  } deriving (Show)

newtype FFIMarshalState = FFIMarshalState
  { ffiDecls :: IM.IntMap FFIDecl
  } deriving (Show)

emptyFFIMarshalState :: FFIMarshalState
emptyFFIMarshalState = FFIMarshalState {ffiDecls = mempty}

marshalToFFIValueType :: GHC.Located GHC.RdrName -> Maybe FFIValueType
marshalToFFIValueType loc_t =
  case GHC.occNameString $ GHC.rdrNameOcc $ GHC.unLoc loc_t of
    "Float" -> Just FFI_F32
    "Double" -> Just FFI_F64
    "JSRef" -> Just FFI_REF
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
    GHC.HsAppsTy _ [c', t'] ->
      case (GHC.unLoc c', GHC.unLoc t') of
        (GHC.HsAppPrefix _ c, GHC.HsAppPrefix _ t) ->
          case GHC.unLoc c of
            GHC.HsTyVar _ _ loc_io
              | GHC.occNameString (GHC.rdrNameOcc $ GHC.unLoc loc_io) == "IO" -> do
                r <-
                  case GHC.unLoc t of
                    GHC.HsTyVar _ _ loc_t ->
                      Just <$> marshalToFFIValueType loc_t
                    GHC.HsTupleTy _ _ [] -> Just Nothing
                    _ -> Nothing
                pure
                  FFIFunctionType
                    {ffiParamTypes = [], ffiResultType = r, ffiInIO = True}
            _ -> Nothing
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
    Just FFI_F32 -> mkQualTyCon "GHC.Types" "Float"
    Just FFI_F64 -> mkQualTyCon "GHC.Types" "Double"
    Just FFI_REF -> mkQualTyCon "GHC.Types" "Double"
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

recoverWasmValueTypeName :: Maybe FFIValueType -> SBS.ShortByteString
recoverWasmValueTypeName vt =
  case vt of
    Just FFI_F32 -> "F32"
    Just FFI_F64 -> "F64"
    Just FFI_REF -> "F64"
    Nothing -> "None"

recoverWasmValueType :: Maybe FFIValueType -> ValueType
recoverWasmValueType vt =
  case vt of
    Just FFI_F32 -> F32
    Just FFI_F64 -> F64
    Just FFI_REF -> F64
    Nothing -> None

recoverWasmTypeName :: FFIFunctionType -> SBS.ShortByteString
recoverWasmTypeName FFIFunctionType {..} =
  recoverWasmValueTypeName ffiResultType <> "(" <>
  (case ffiParamTypes of
     [] -> ""
     [x] -> recoverWasmValueTypeName $ Just x
     x:xs ->
       recoverWasmValueTypeName (Just x) <>
       mconcat ["," <> recoverWasmValueTypeName (Just x') | x' <- xs]) <>
  ")"

recoverCCallTarget :: Int -> String
recoverCCallTarget = ("__asterius_jsffi_" <>) . show

processJSFFI :: Data a => a -> State FFIMarshalState a
processJSFFI t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep (GHC.ForeignDecl GHC.GhcPs)) of
    Just HRefl ->
      case t of
        GHC.ForeignImport {fd_fi = GHC.CImport loc_conv _ _ _ loc_src, ..} ->
          case GHC.unLoc loc_conv of
            GHC.JavaScriptCallConv -> do
              FFIMarshalState {..} <- get
              let new_k = maybe 0 (succ . fst) $ IM.lookupMax ffiDecls
                  new_decls = IM.insert new_k new_decl ffiDecls
              put $ FFIMarshalState {ffiDecls = new_decls}
              pure
                t
                  { GHC.fd_sig_ty =
                      GHC.mkHsImplicitBndrs $ recoverHsFunctionType ffi_ftype
                  , GHC.fd_fi =
                      GHC.CImport
                        (GHC.noLoc GHC.CCallConv)
                        (GHC.noLoc GHC.PlayRisky)
                        Nothing
                        (GHC.CFunction $
                         GHC.StaticTarget
                           GHC.NoSourceText
                           (GHC.mkFastString $ recoverCCallTarget new_k)
                           Nothing
                           True)
                        (GHC.noLoc GHC.NoSourceText)
                  }
              where GHC.SourceText src = GHC.unLoc loc_src
                    Just ffi_ftype =
                      marshalToFFIFunctionType $ GHC.hsImplicitBody fd_sig_ty
                    Right chunks =
                      parseOnly parseFFIChunks $ CBS.pack $ read src
                    new_decl =
                      FFIDecl
                        {ffiFunctionType = ffi_ftype, ffiSourceChunks = chunks}
            _ -> pure t
        _ -> pure t
    _ -> gmapM processJSFFI t

collectJSFFISrc ::
     GHC.HsParsedModule
  -> FFIMarshalState
  -> (GHC.HsParsedModule, FFIMarshalState)
collectJSFFISrc m ffi_state = (m {GHC.hpm_module = rewriteJSRef imp_m}, st)
  where
    (new_m, st) = runState (processJSFFI $ GHC.hpm_module m) ffi_state
    imp_m
      | IM.null (ffiDecls st) = new_m
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

addJSFFIProcessor :: Compiler -> IO (Compiler, IO FFIMarshalState)
addJSFFIProcessor c = do
  ffi_state_ref <- newIORef FFIMarshalState {ffiDecls = mempty}
  pure
    ( c
        { patch =
            \mod_summary parsed_mod -> do
              patched_mod <-
                liftIO $
                atomicModifyIORef' ffi_state_ref $ \ffi_state ->
                  let (patched_mod, ffi_state') =
                        collectJSFFISrc parsed_mod ffi_state
                   in (ffi_state', patched_mod)
              patch c mod_summary patched_mod
        }
    , readIORef ffi_state_ref)

generateFFIFunctionTypeMap ::
     FFIMarshalState -> HM.HashMap SBS.ShortByteString FunctionType
generateFFIFunctionTypeMap FFIMarshalState {..} =
  HM.fromList
    [ ( recoverWasmTypeName ffiFunctionType
      , FunctionType
          { returnType = recoverWasmValueType $ ffiResultType ffiFunctionType
          , paramTypes =
              V.fromList
                [ recoverWasmValueType (Just t)
                | t <- ffiParamTypes ffiFunctionType
                ]
          })
    | FFIDecl {..} <- IM.elems ffiDecls
    ]

generateFFIFunctionImports :: FFIMarshalState -> V.Vector FunctionImport
generateFFIFunctionImports FFIMarshalState {..} =
  V.fromList
    [ FunctionImport
      { internalName = fn
      , externalModuleName = "jsffi"
      , externalBaseName = fn
      , functionTypeName = recoverWasmTypeName ffiFunctionType
      }
    | (k, FFIDecl {..}) <- IM.toList ffiDecls
    , let fn = fromString $ recoverCCallTarget k
    ]

generateJSFFILambda :: FFIDecl -> Builder
generateJSFFILambda FFIDecl {ffiFunctionType = FFIFunctionType {..}, ..} =
  "((" <>
  mconcat (intersperse "," ["_" <> intDec i | i <- [1 .. length ffiParamTypes]]) <>
  ")=>(" <>
  mconcat
    [ case chunk of
      Lit s -> string7 s
      Field i -> "_" <> intDec i
    | chunk <- ffiSourceChunks
    ] <>
  "))"

generateJSFFIDict :: FFIMarshalState -> Builder
generateJSFFIDict FFIMarshalState {..} =
  "{" <>
  mconcat
    (intersperse
       ","
       [ string7 (recoverCCallTarget k) <> ":" <> generateJSFFILambda ffi_decl
       | (k, ffi_decl) <- IM.toList ffiDecls
       ]) <>
  "}"
