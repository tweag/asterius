{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.Foreign.Internals
  ( FFIHookState (..),
    globalFFIHookState,
    processFFIImport,
    processFFIExport,
    getFFIFunctionType,
  )
where

import Asterius.Foreign.SupportedTypes
import Asterius.Internals.Name
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Asterius.TypesConv
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as M
import Data.String
import qualified ForeignCall as GHC
import qualified GhcPlugins as GHC
import qualified HsSyn as GHC
import qualified PrelNames as GHC
import System.IO.Unsafe
import qualified TcRnMonad as GHC
import qualified TyCoRep as GHC

getFFIFunctionType :: GHC.Type -> Either String FFIFunctionType
getFFIFunctionType norm_sig_ty = case res_ty of
  GHC.FunTy norm_t1 norm_t2 -> do
    vt <- getFFIValueType norm_t1
    ft <- getFFIFunctionType norm_t2
    pure ft {ffiParamTypes = vt : ffiParamTypes ft}
  GHC.TyConApp norm_tc norm_tys1 | GHC.getName norm_tc == GHC.ioTyConName ->
    case norm_tys1 of
      [GHC.TyConApp u []]
        | u == GHC.unitTyCon ->
          pure
            FFIFunctionType
              { ffiParamTypes = [],
                ffiResultTypes = [],
                ffiInIO = True
              }
      [norm_t1] -> do
        r <- getFFIValueType norm_t1
        pure
          FFIFunctionType
            { ffiParamTypes = [],
              ffiResultTypes = [r],
              ffiInIO = True
            }
      _ ->
        Left $
          "Asterius.Foreign.Internals.getFFIFunctionType: "
            <> GHC.showPpr GHC.unsafeGlobalDynFlags norm_sig_ty
  _ -> do
    r <- getFFIValueType res_ty
    pure
      FFIFunctionType
        { ffiParamTypes = [],
          ffiResultTypes = [r],
          ffiInIO = False
        }
  where
    res_ty = GHC.dropForAlls norm_sig_ty

newtype FFIHookState = FFIHookState
  { ffiHookState :: M.Map GHC.Module FFIMarshalState
  }

{-# NOINLINE globalFFIHookState #-}
globalFFIHookState :: IORef FFIHookState
globalFFIHookState =
  unsafePerformIO $ newIORef FFIHookState {ffiHookState = M.empty}

processFFIImport ::
  IORef FFIHookState ->
  GHC.Type ->
  GHC.ForeignImport ->
  GHC.TcM GHC.ForeignImport
processFFIImport hook_state_ref norm_sig_ty (GHC.CImport (GHC.unLoc -> GHC.JavaScriptCallConv) loc_safety _ (GHC.CFunction _) (GHC.unLoc -> GHC.SourceText src)) =
  do
    dflags <- GHC.getDynFlags
    ms_mod <- GHC.getModule
    u <- GHC.getUniqueM
    let ffi_ftype = case getFFIFunctionType norm_sig_ty of
          Right r -> r
          Left err -> error err
        ffi_safety = case GHC.unLoc loc_safety of
          GHC.PlaySafe | GHC.isGoodSrcSpan $ GHC.getLoc loc_safety -> FFISafe
          GHC.PlayInterruptible -> FFIInterruptible
          _ -> FFIUnsafe
        ffi_safety_prefix
          | ffi_safety == FFIUnsafe = "__asterius_jsffi_"
          | otherwise = "__asterius_jsffi_async_"
        ffi_src_text = read src
        new_k =
          ffi_safety_prefix <> zEncodeModule ms_mod <> "_" <> asmPpr dflags u
        new_decl =
          FFIImportDecl
            { ffiFunctionType = ffi_ftype,
              ffiSafety = ffi_safety,
              ffiSourceText = ffi_src_text
            }
        new_conv
          | ffi_safety == FFIUnsafe = GHC.CCallConv
          | otherwise = GHC.PrimCallConv
        alter_hook_state (Just ffi_state) =
          Just
            ffi_state
              { ffiImportDecls =
                  SM.insert (fromString new_k) new_decl $
                    ffiImportDecls ffi_state
              }
        alter_hook_state _ =
          Just
            mempty
              { ffiImportDecls = SM.singleton (fromString new_k) new_decl
              }
    liftIO $
      atomicModifyIORef' hook_state_ref $ \hook_state ->
        ( hook_state
            { ffiHookState =
                M.alter alter_hook_state ms_mod $
                  ffiHookState hook_state
            },
          ()
        )
    pure $
      GHC.CImport
        (GHC.noLoc new_conv)
        (GHC.noLoc GHC.PlayRisky)
        Nothing
        ( GHC.CFunction $
            GHC.StaticTarget
              GHC.NoSourceText
              (GHC.mkFastString $ new_k <> "_wrapper")
              (Just GHC.rtsUnitId)
              True
        )
        (GHC.noLoc GHC.NoSourceText)
processFFIImport _ _ imp_decl = pure imp_decl

processFFIExport ::
  IORef FFIHookState ->
  GHC.Type ->
  GHC.Id ->
  GHC.ForeignExport ->
  GHC.TcM GHC.ForeignExport
processFFIExport hook_state_ref norm_sig_ty export_id (GHC.CExport (GHC.unLoc -> GHC.CExportStatic src_txt lbl GHC.JavaScriptCallConv) loc_src) =
  do
    dflags <- GHC.getDynFlags
    ms_mod <- GHC.getModule
    let ffi_ftype = case getFFIFunctionType norm_sig_ty of
          Right r -> r
          Left err -> error err
        new_k = mkEntitySymbol $ GHC.fastStringToByteString lbl
        export_closure = idClosureSymbol dflags export_id
        new_decl =
          FFIExportDecl
            { ffiFunctionType = ffi_ftype,
              ffiExportClosure = export_closure
            }
        alter_hook_state (Just ffi_state) =
          Just
            ffi_state
              { ffiExportDecls = SM.insert new_k new_decl $ ffiExportDecls ffi_state
              }
        alter_hook_state _ =
          Just mempty {ffiExportDecls = SM.singleton new_k new_decl}
    liftIO $
      atomicModifyIORef' hook_state_ref $ \hook_state ->
        ( hook_state
            { ffiHookState =
                M.alter alter_hook_state ms_mod $
                  ffiHookState hook_state
            },
          ()
        )
    pure $
      GHC.CExport
        (GHC.noLoc $ GHC.CExportStatic src_txt lbl GHC.CCallConv)
        loc_src
processFFIExport _ _ _ exp_decl = pure exp_decl
