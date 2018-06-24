{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.FFI
  ( Chunk(..)
  , FFIType(..)
  , FFIDecl(..)
  , FFIMarshalState(..)
  , addJSFFIProcessor
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.State.Strict
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as CBS
import Data.Data (Data, gmapM, gmapT)
import Data.Functor
import qualified Data.IntMap.Strict as IM
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

data FFIType
  = FFI_I32
  | FFI_F32
  | FFI_F64
  | FFI_REF
  deriving (Show)

data FFIDecl = FFIDecl
  { ffiTypeSpine :: [FFIType]
  , ffiSourceChunks :: [Chunk Int]
  } deriving (Show)

newtype FFIMarshalState = FFIMarshalState
  { ffiDecls :: IM.IntMap FFIDecl
  } deriving (Show)

generateFFITypeSpine :: GHC.LHsType GHC.GhcPs -> Maybe [FFIType]
generateFFITypeSpine loc_ty =
  case ty of
    GHC.HsTyVar _ _ loc_t ->
      case GHC.occNameString $ GHC.rdrNameOcc $ GHC.unLoc loc_t of
        "Int32" -> Just [FFI_I32]
        "Float" -> Just [FFI_F32]
        "Double" -> Just [FFI_F64]
        "JSRef" -> Just [FFI_REF]
        _ -> Nothing
    GHC.HsFunTy _ t0 t1 -> do
      s0 <- generateFFITypeSpine t0
      s1 <- generateFFITypeSpine t1
      pure $ s0 <> s1
    _ -> Nothing
  where
    ty = GHC.unLoc loc_ty

rewriteJSRef :: Data a => a -> a
rewriteJSRef t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep GHC.RdrName) of
    Just HRefl ->
      case t of
        GHC.Unqual n
          | GHC.occNameString n == "JSRef" ->
            GHC.Qual (GHC.mkModuleName "Data.Int") (GHC.mkTcOcc "Int32")
        _ -> t
    _ -> gmapT rewriteJSRef t

mkQualTyCon :: String -> String -> GHC.LHsType GHC.GhcPs
mkQualTyCon m t =
  GHC.noLoc $
  GHC.HsTyVar GHC.NoExt GHC.NotPromoted $
  GHC.noLoc $ GHC.Qual (GHC.mkModuleName m) (GHC.mkTcOcc t)

recoverHsType :: FFIType -> GHC.LHsType GHC.GhcPs
recoverHsType t =
  case t of
    FFI_I32 -> mkQualTyCon "Data.Int" "Int32"
    FFI_F32 -> mkQualTyCon "GHC.Types" "Float"
    FFI_F64 -> mkQualTyCon "GHC.Types" "Double"
    FFI_REF -> mkQualTyCon "Data.Int" "Int32"

recoverHsTypeFromSpine :: [FFIType] -> GHC.LHsType GHC.GhcPs
recoverHsTypeFromSpine [] = error "Empty spine"
recoverHsTypeFromSpine [r] =
  GHC.noLoc $
  GHC.HsAppTy GHC.NoExt (mkQualTyCon "GHC.Types" "IO") (recoverHsType r)
recoverHsTypeFromSpine (t:ts) =
  GHC.noLoc $
  GHC.HsFunTy GHC.NoExt (recoverHsType t) (recoverHsTypeFromSpine ts)

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
                      GHC.mkHsImplicitBndrs $ recoverHsTypeFromSpine spine
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
                    Just spine =
                      generateFFITypeSpine (GHC.hsImplicitBody fd_sig_ty)
                    Right chunks =
                      parseOnly parseFFIChunks $ CBS.pack $ read src
                    new_decl =
                      FFIDecl {ffiTypeSpine = spine, ffiSourceChunks = chunks}
            _ -> pure t
        _ -> pure t
    _ -> gmapM processJSFFI t

collectJSFFISrc :: GHC.HsParsedModule -> (GHC.HsParsedModule, FFIMarshalState)
collectJSFFISrc m = (m {GHC.hpm_module = rewriteJSRef imp_m}, st)
  where
    (new_m, st) =
      runState
        (processJSFFI $ GHC.hpm_module m)
        FFIMarshalState {ffiDecls = mempty}
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
              | imp <- ["Data.Int", "GHC.Types"]
              ] <>
              GHC.hsmodImports new_m_unloc
          }
      where
        new_m_unloc = GHC.unLoc new_m

addJSFFIProcessor :: Compiler -> IO (Compiler, IO FFIMarshalState)
addJSFFIProcessor c = do
  ffi_state_ref <- newEmptyMVar
  pure
    ( c
        { patch =
            \mod_summary parsed_mod -> do
              let (patched_mod, ffi_state) = collectJSFFISrc parsed_mod
              liftIO $ putMVar ffi_state_ref ffi_state
              patch c mod_summary patched_mod
        }
    , takeMVar ffi_state_ref)
