{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.Foreign.Internals
  ( FFIHookState (..),
    globalFFIHookState,
    processFFIImport,
    processFFIExport
    )
where

import Asterius.Types
import Asterius.TypesConv
import qualified CLabel as GHC
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString.Short as SBS
import Data.Functor
import Data.IORef
import qualified Data.Map.Strict as M
import Data.String
import qualified ForeignCall as GHC
import qualified GhcPlugins as GHC
import qualified HsSyn as GHC
import qualified PrelNames as GHC
import System.IO.Unsafe
import qualified TcRnMonad as GHC
import Text.Parsec
  ( anyChar,
    char,
    digit,
    parse,
    try
    )
import Text.Parsec.String (Parser)
import qualified TyCoRep as GHC
import qualified TysPrim as GHC

ffiBoxedValueTypeMap0,
  ffiBoxedValueTypeMap1,
  ffiPrimValueTypeMap0,
  ffiPrimValueTypeMap1,
  ffiValueTypeMap0,
  ffiValueTypeMap1
    :: GHC.NameEnv FFIValueType
ffiBoxedValueTypeMap0 =
  GHC.mkNameEnv
    [ ( GHC.charTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Char",
            signed = False
            }
        ),
      ( GHC.boolTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Bool",
            signed = False
            }
        ),
      ( GHC.intTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Int",
            signed = True
            }
        ),
      ( GHC.wordTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Word",
            signed = False
            }
        ),
      ( GHC.floatTyConName,
        FFI_VAL
          { ffiWasmValueType = F32,
            ffiJSValueType = F32,
            hsTyCon = "Float",
            signed = True
            }
        ),
      ( GHC.doubleTyConName,
        FFI_VAL
          { ffiWasmValueType = F64,
            ffiJSValueType = F64,
            hsTyCon = "Double",
            signed = True
            }
        )
      ]

ffiBoxedValueTypeMap1 =
  GHC.mkNameEnv
    [ ( GHC.ptrTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "Ptr",
            signed = False
            }
        ),
      ( GHC.funPtrTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "FunPtr",
            signed = False
            }
        ),
      ( GHC.stablePtrTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "StablePtr",
            signed = False
            }
        )
      ]

ffiPrimValueTypeMap0 =
  GHC.mkNameEnv
    [ ( GHC.charPrimTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "",
            signed = False
            }
        ),
      ( GHC.intPrimTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "",
            signed = True
            }
        ),
      ( GHC.wordPrimTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "",
            signed = False
            }
        ),
      ( GHC.floatPrimTyConName,
        FFI_VAL
          { ffiWasmValueType = F32,
            ffiJSValueType = F32,
            hsTyCon = "",
            signed = True
            }
        ),
      ( GHC.doublePrimTyConName,
        FFI_VAL
          { ffiWasmValueType = F64,
            ffiJSValueType = F64,
            hsTyCon = "",
            signed = True
            }
        )
      ]

ffiPrimValueTypeMap1 =
  GHC.mkNameEnv
    [ ( GHC.addrPrimTyConName,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "",
            signed = False
            }
        ),
      ( GHC.getName GHC.stablePtrPrimTyCon,
        FFI_VAL
          { ffiWasmValueType = I64,
            ffiJSValueType = F64,
            hsTyCon = "",
            signed = False
            }
        )
      ]

ffiValueTypeMap0 = ffiBoxedValueTypeMap0 `GHC.plusNameEnv` ffiPrimValueTypeMap0

ffiValueTypeMap1 = ffiBoxedValueTypeMap1 `GHC.plusNameEnv` ffiPrimValueTypeMap1

parseFFIValueType :: Bool -> GHC.Type -> GHC.Type -> Maybe FFIValueType
parseFFIValueType accept_prim sig_ty norm_sig_ty =
  case (sig_ty, norm_sig_ty) of
    (GHC.TyConApp _ _, GHC.TyConApp norm_tc []) ->
      GHC.lookupNameEnv ffi_valuetype_map0 (GHC.getName norm_tc)
    (GHC.TyConApp tc _, GHC.TyConApp norm_tc [_])
      | take 2 (GHC.occNameString (GHC.getOccName tc))
          == "JS"
          && GHC.getName norm_tc
          == GHC.stablePtrTyConName ->
        pure FFI_JSVAL
      | otherwise ->
        GHC.lookupNameEnv ffi_valuetype_map1 (GHC.getName norm_tc)
    _ -> Nothing
  where
    ffi_valuetype_map0
      | accept_prim = ffiValueTypeMap0
      | otherwise = ffiBoxedValueTypeMap0
    ffi_valuetype_map1
      | accept_prim = ffiValueTypeMap1
      | otherwise = ffiBoxedValueTypeMap1

parseFFIFunctionType :: Bool -> GHC.Type -> GHC.Type -> Maybe FFIFunctionType
parseFFIFunctionType accept_prim sig_ty norm_sig_ty =
  case (sig_ty, norm_sig_ty) of
    (GHC.FunTy t1 t2, GHC.FunTy norm_t1 norm_t2) -> do
      vt <- parseFFIValueType accept_prim t1 norm_t1
      ft <- parseFFIFunctionType accept_prim t2 norm_t2
      pure ft {ffiParamTypes = vt : ffiParamTypes ft}
    (GHC.TyConApp _ tys1, GHC.TyConApp norm_tc norm_tys1)
      | GHC.getName norm_tc == GHC.ioTyConName ->
        case (tys1, norm_tys1) of
          (_, [GHC.TyConApp u []])
            | u == GHC.unitTyCon ->
              pure
                FFIFunctionType
                  { ffiParamTypes = [],
                    ffiResultTypes = [],
                    ffiInIO = True
                    }
          ([t1], [norm_t1]) -> do
            r <- parseFFIValueType False t1 norm_t1
            pure
              FFIFunctionType
                { ffiParamTypes = [],
                  ffiResultTypes = [r],
                  ffiInIO = True
                  }
          _ -> Nothing
    _ -> do
      r <- parseFFIValueType accept_prim sig_ty norm_sig_ty
      pure
        FFIFunctionType
          { ffiParamTypes = [],
            ffiResultTypes = [r],
            ffiInIO = False
            }

parseField :: Parser a -> Parser (Chunk a)
parseField f = do
  void $ char '$'
  void $ char '{'
  v <- f
  void $ char '}'
  pure $ Field v

parseChunk :: Parser (Chunk a) -> Parser (Chunk a)
parseChunk f = try f <|> do
  c <- anyChar
  pure $ Lit [c]

parseChunks :: Parser (Chunk a) -> Parser [Chunk a]
parseChunks = many

combineChunks :: [Chunk a] -> [Chunk a]
combineChunks =
  foldr
    ( curry $ \case
        (Lit l, Lit l' : cs) -> Lit (l <> l') : cs
        (c, cs) -> c : cs
      )
    []

parseFFIChunks :: Parser [Chunk Int]
parseFFIChunks =
  combineChunks <$> parseChunks (parseChunk (parseField (read <$> some digit)))

newtype FFIHookState = FFIHookState {ffiHookState :: M.Map AsteriusModuleSymbol FFIMarshalState}

{-# NOINLINE globalFFIHookState #-}
globalFFIHookState :: IORef FFIHookState
globalFFIHookState =
  unsafePerformIO $ newIORef FFIHookState {ffiHookState = M.empty}

processFFIImport
  :: IORef FFIHookState
  -> GHC.Type
  -> GHC.Type
  -> GHC.ForeignImport
  -> GHC.TcM GHC.ForeignImport
processFFIImport hook_state_ref sig_ty norm_sig_ty (GHC.CImport (GHC.unLoc -> GHC.JavaScriptCallConv) loc_safety _ _ (GHC.unLoc -> GHC.SourceText src)) =
  do
    dflags <- GHC.getDynFlags
    mod_sym <- marshalToModuleSymbol <$> GHC.getModule
    u <- GHC.getUniqueM
    let Just ffi_ftype = parseFFIFunctionType True sig_ty norm_sig_ty
        ffi_safety = case GHC.unLoc loc_safety of
          GHC.PlaySafe | GHC.isGoodSrcSpan $ GHC.getLoc loc_safety -> FFISafe
          GHC.PlayInterruptible -> FFIInterruptible
          _ -> FFIUnsafe
        ffi_safety_prefix
          | ffi_safety == FFIUnsafe = "__asterius_jsffi_"
          | otherwise = "__asterius_jsffi_async_"
        Right chunks = parse parseFFIChunks src (read src)
        new_k =
          ffi_safety_prefix
            <> zEncodeModuleSymbol mod_sym
            <> "_"
            <> asmPpr dflags u
        new_decl = FFIImportDecl
          { ffiFunctionType = ffi_ftype,
            ffiSafety = ffi_safety,
            ffiSourceChunks = chunks
            }
        alter_hook_state (Just ffi_state) =
          Just
            ffi_state
              { ffiImportDecls = M.insert (fromString new_k) new_decl
                  $ ffiImportDecls ffi_state
                }
        alter_hook_state _ =
          Just mempty {ffiImportDecls = M.singleton (fromString new_k) new_decl}
    liftIO $ atomicModifyIORef' hook_state_ref $ \hook_state ->
      ( hook_state
          { ffiHookState = M.alter alter_hook_state mod_sym
              $ ffiHookState hook_state
            },
        ()
        )
    pure
      $ GHC.CImport
          (GHC.noLoc GHC.CCallConv)
          (GHC.noLoc GHC.PlayRisky)
          Nothing
          ( GHC.CFunction
              $ GHC.StaticTarget
                  GHC.NoSourceText
                  (GHC.mkFastString $ new_k <> "_wrapper")
                  Nothing
                  True
            )
          (GHC.noLoc GHC.NoSourceText)
processFFIImport _ _ _ imp_decl = pure imp_decl

processFFIExport
  :: IORef FFIHookState
  -> GHC.Type
  -> GHC.Type
  -> GHC.Id
  -> GHC.ForeignExport
  -> GHC.TcM GHC.ForeignExport
processFFIExport hook_state_ref sig_ty norm_sig_ty export_id (GHC.CExport (GHC.unLoc -> GHC.CExportStatic src_txt lbl GHC.JavaScriptCallConv) loc_src) =
  do
    dflags <- GHC.getDynFlags
    mod_sym <- marshalToModuleSymbol <$> GHC.getModule
    let Just ffi_ftype = parseFFIFunctionType False sig_ty norm_sig_ty
        new_k = AsteriusEntitySymbol
          { entityName = SBS.toShort $ GHC.fastStringToByteString lbl
            }
        export_closure =
          fromString $ asmPpr dflags
            $ GHC.mkClosureLabel
                (GHC.getName export_id)
                GHC.NoCafRefs
        new_decl = FFIExportDecl
          { ffiFunctionType = ffi_ftype,
            ffiExportClosure = export_closure
            }
        alter_hook_state (Just ffi_state) =
          Just
            ffi_state
              { ffiExportDecls = M.insert new_k new_decl $ ffiExportDecls ffi_state
                }
        alter_hook_state _ =
          Just mempty {ffiExportDecls = M.singleton new_k new_decl}
    liftIO $ atomicModifyIORef' hook_state_ref $ \hook_state ->
      ( hook_state
          { ffiHookState = M.alter alter_hook_state mod_sym
              $ ffiHookState hook_state
            },
        ()
        )
    pure
      $ GHC.CExport
          (GHC.noLoc $ GHC.CExportStatic src_txt lbl GHC.CCallConv)
          loc_src
processFFIExport _ _ _ _ exp_decl = pure exp_decl
