{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.FFI where

import Control.Applicative
import Control.Monad.Writer.Strict
import Data.Attoparsec.ByteString.Char8
import Data.Data (Data, gmapM)
import Data.Functor
import qualified ForeignCall as GHC
import qualified GhcPlugins as GHC
import qualified HsSyn as GHC
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

mkQualDataCon :: String -> String -> GHC.LHsType GHC.GhcPs
mkQualDataCon m t =
  GHC.noLoc $
  GHC.HsTyVar GHC.NoExt GHC.NotPromoted $
  GHC.noLoc $ GHC.Qual (GHC.mkModuleName m) (GHC.mkDataOcc t)

recoverHsType :: FFIType -> GHC.LHsType GHC.GhcPs
recoverHsType t =
  case t of
    FFI_I32 -> mkQualDataCon "Data.Int" "Int32"
    FFI_F32 -> mkQualDataCon "GHC.Types" "Float"
    FFI_F64 -> mkQualDataCon "GHC.Types" "Double"
    FFI_REF -> mkQualDataCon "Data.Int" "Int32"

recoverHsTypeFromSpine :: [FFIType] -> GHC.LHsType GHC.GhcPs
recoverHsTypeFromSpine [] = error "Empty spine"
recoverHsTypeFromSpine [r] =
  GHC.noLoc $
  GHC.HsAppTy GHC.NoExt (mkQualDataCon "GHC.Types" "IO") (recoverHsType r)
recoverHsTypeFromSpine (t:ts) =
  GHC.noLoc $
  GHC.HsFunTy GHC.NoExt (recoverHsType t) (recoverHsTypeFromSpine ts)

processJSFFI :: Data a => a -> Writer [([FFIType], String)] a
processJSFFI t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep (GHC.ForeignDecl GHC.GhcPs)) of
    Just HRefl ->
      case t of
        GHC.ForeignImport {fd_fi = GHC.CImport loc_conv _ _ _ loc_src, ..} ->
          case conv of
            GHC.JavaScriptCallConv ->
              tell [(spine, src)] $>
              t
                { GHC.fd_sig_ty =
                    GHC.mkHsImplicitBndrs $ recoverHsTypeFromSpine spine
                , GHC.fd_fi =
                    GHC.CImport
                      (GHC.noLoc GHC.CCallConv)
                      (GHC.noLoc GHC.PlayRisky)
                      Nothing
                      undefined
                      (GHC.noLoc GHC.NoSourceText)
                }
            _ -> pure t
          where conv = GHC.unLoc loc_conv
                GHC.SourceText src = GHC.unLoc loc_src
                Just spine = generateFFITypeSpine (GHC.hsImplicitBody fd_sig_ty)
        _ -> pure t
    _ -> gmapM processJSFFI t

collectJSFFISrc ::
     GHC.HsParsedModule -> (GHC.HsParsedModule, [([FFIType], String)])
collectJSFFISrc m = (m {GHC.hpm_module = new_m}, src)
  where
    (new_m, src) = runWriter (processJSFFI $ GHC.hpm_module m)
