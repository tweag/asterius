{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Resolve
  ( unresolvedGlobalRegType
  , LinkReport(..)
  , linkStart
  ) where

import Asterius.Builtins
import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.JSFFI
import Asterius.Passes.All
import Asterius.Passes.DataSymbolTable
import Asterius.Passes.Events
import Asterius.Passes.FunctionSymbolTable
import Asterius.Types
import Control.Monad.State.Strict
import Data.Binary
import Data.Data (Data, gmapQl)
import Data.List
import qualified Data.Map.Lazy as LM
import qualified Data.Set as S
import Foreign
import GHC.Generics
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)
import Type.Reflection ((:~~:)(..), TypeRep, eqTypeRep, typeOf, typeRep)
import Unsafe.Coerce

unresolvedGlobalRegType :: UnresolvedGlobalReg -> ValueType
unresolvedGlobalRegType gr =
  case gr of
    FloatReg _ -> F32
    DoubleReg _ -> F64
    _ -> I64

collectAsteriusEntitySymbols ::
     Data a => a -> S.Set AsteriusEntitySymbol -> S.Set AsteriusEntitySymbol
collectAsteriusEntitySymbols t acc =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep AsteriusEntitySymbol) of
    Just HRefl -> S.insert t acc
    _ -> gmapQl (.) id collectAsteriusEntitySymbols t acc

data LinkReport = LinkReport
  { unavailableSymbols :: S.Set AsteriusEntitySymbol
  , staticsSymbolMap, functionSymbolMap :: LM.Map AsteriusEntitySymbol Int64
  , infoTableSet :: S.Set Int64
  , staticMBlocks :: Int
  , bundledFFIMarshalState :: FFIMarshalState
  } deriving (Generic, Show)

instance Binary LinkReport

instance Semigroup LinkReport where
  r0 <> r1 =
    LinkReport
      { unavailableSymbols = unavailableSymbols r0 <> unavailableSymbols r1
      , staticsSymbolMap = staticsSymbolMap r0 <> staticsSymbolMap r1
      , functionSymbolMap = functionSymbolMap r0 <> functionSymbolMap r1
      , infoTableSet = infoTableSet r0 <> infoTableSet r1
      , staticMBlocks = 0
      , bundledFFIMarshalState =
          bundledFFIMarshalState r0 <> bundledFFIMarshalState r1
      }

instance Monoid LinkReport where
  mempty =
    LinkReport
      { unavailableSymbols = mempty
      , staticsSymbolMap = mempty
      , functionSymbolMap = mempty
      , infoTableSet = mempty
      , staticMBlocks = 0
      , bundledFFIMarshalState = mempty
      }

mergeSymbols ::
     Bool
  -> AsteriusModule
  -> S.Set AsteriusEntitySymbol
  -> (AsteriusModule, LinkReport)
mergeSymbols debug store_mod root_syms =
  (final_m, final_rep {bundledFFIMarshalState = ffi_this})
  where
    ffi_all = ffiMarshalState store_mod
    ffi_this =
      ffi_all
        { ffiImportDecls =
            flip LM.filterWithKey (ffiImportDecls ffi_all) $ \k _ ->
              (k <> "_wrapper") `LM.member` functionMap final_m
        }
    (_, _, final_rep, final_m) = go (root_syms, S.empty, mempty, mempty)
    go i@(i_staging_syms, _, _, _)
      | S.null i_staging_syms = i
      | otherwise = go $ iter i
    iter (i_staging_syms, i_acc_syms, i_rep, i_m) =
      (o_staging_syms, o_acc_syms, o_rep, o_m)
      where
        o_acc_syms = i_staging_syms <> i_acc_syms
        (o_unavailable_syms, i_child_syms, o_m) =
          S.foldr'
            (\i_staging_sym (i_unavailable_syms_acc, i_child_syms_acc, o_m_acc) ->
               case LM.lookup i_staging_sym (staticsMap store_mod) of
                 Just ss ->
                   ( i_unavailable_syms_acc
                   , collectAsteriusEntitySymbols ss i_child_syms_acc
                   , o_m_acc
                       { staticsMap =
                           LM.insert i_staging_sym ss (staticsMap o_m_acc)
                       })
                 _ ->
                   case LM.lookup i_staging_sym (functionMap store_mod) of
                     Just func ->
                       ( i_unavailable_syms_acc
                       , collectAsteriusEntitySymbols func i_child_syms_acc
                       , o_m_acc
                           { functionMap =
                               LM.insert
                                 i_staging_sym
                                 func
                                 (functionMap o_m_acc)
                           })
                     _
                       | LM.member i_staging_sym (functionErrorMap store_mod) ->
                         ( i_unavailable_syms_acc
                         , i_child_syms_acc
                         , o_m_acc
                             { functionMap =
                                 LM.insert
                                   i_staging_sym
                                   AsteriusFunction
                                     { functionType =
                                         FunctionType
                                           { paramTypes = []
                                           , returnTypes = [I64]
                                           }
                                     , body =
                                         emitErrorMessage [I64] $
                                         entityName i_staging_sym <>
                                         " failed: it was marked as broken by code generator, with error message: " <>
                                         showSBS
                                           (functionErrorMap store_mod !
                                            i_staging_sym)
                                     }
                                   (functionMap o_m_acc)
                             })
                       | otherwise ->
                         ( S.insert i_staging_sym i_unavailable_syms_acc
                         , i_child_syms_acc
                         , o_m_acc))
            (unavailableSymbols i_rep, S.empty, i_m)
            i_staging_syms
        o_rep = i_rep {unavailableSymbols = o_unavailable_syms}
        o_staging_syms = i_child_syms `S.difference` o_acc_syms

makeInfoTableSet ::
     AsteriusModule -> LM.Map AsteriusEntitySymbol Int64 -> S.Set Int64
makeInfoTableSet AsteriusModule {..} sym_map =
  S.map (sym_map !) $
  LM.keysSet $ LM.filter ((== InfoTable) . staticsType) staticsMap

resolveAsteriusModule ::
     Bool
  -> Bool
  -> FFIMarshalState
  -> [AsteriusEntitySymbol]
  -> AsteriusModule
  -> Int64
  -> Int64
  -> ( Module
     , LM.Map AsteriusEntitySymbol Int64
     , LM.Map AsteriusEntitySymbol Int64
     , [Event]
     , Int)
resolveAsteriusModule debug has_main bundled_ffi_state export_funcs m_globals_resolved func_start_addr data_start_addr =
  (new_mod, ss_sym_map, func_sym_map, err_msgs, initial_mblocks)
  where
    (func_sym_map, _) =
      makeFunctionSymbolTable m_globals_resolved func_start_addr
    func_table = makeFunctionTable func_sym_map
    (ss_sym_map, last_addr) =
      makeDataSymbolTable m_globals_resolved data_start_addr
    all_sym_map = func_sym_map <> ss_sym_map
    func_imports =
      rtsFunctionImports debug <> generateFFIFunctionImports bundled_ffi_state
    export_func_set = S.fromList export_funcs
    (new_function_map, all_event_map) =
      unsafeCoerce $
      flip runState LM.empty $
      flip LM.traverseWithKey (functionMap m_globals_resolved) $ \sym AsteriusFunction {..} ->
        state $ \event_map ->
          let (body_locals_resolved, local_reg_table, event_map') =
                allPasses
                  debug
                  all_sym_map
                  export_func_set
                  sym
                  functionType
                  event_map
                  body
           in ( Function
                  { functionType = functionType
                  , varTypes = local_reg_table
                  , body = body_locals_resolved
                  }
              , event_map')
    (initial_pages, segs) = makeMemory m_globals_resolved all_sym_map last_addr
    initial_mblocks =
      fromIntegral initial_pages `quot` (mblock_size `quot` wasmPageSize)
    new_mod =
      Module
        { functionMap' = new_function_map
        , functionImports = func_imports
        , functionExports =
            rtsAsteriusFunctionExports debug has_main <>
            [ FunctionExport
              {internalName = "__asterius_jsffi_export_" <> k, externalName = k}
            | k <- map entityName export_funcs
            ]
        , functionTable = func_table
        , memorySegments = segs
        , memoryImport =
            MemoryImport
              { internalName = "__asterius_memory"
              , externalModuleName = "WasmMemory"
              , externalBaseName = "memory"
              , shared = False
              }
        , memoryExport =
            MemoryExport
              {internalName = "__asterius_memory", externalName = "memory"}
        , memoryMBlocks = initial_mblocks
        }
    err_msgs = eventTable all_event_map

linkStart ::
     Bool
  -> Bool
  -> AsteriusModule
  -> S.Set AsteriusEntitySymbol
  -> [AsteriusEntitySymbol]
  -> (Module, [Event], LinkReport)
linkStart debug has_main store root_syms export_funcs =
  ( result_m
  , err_msgs
  , report
      { staticsSymbolMap = ss_sym_map
      , functionSymbolMap = func_sym_map
      , infoTableSet = makeInfoTableSet merged_m ss_sym_map
      , staticMBlocks = static_mbs
      })
  where
    (merged_m, report) =
      mergeSymbols
        debug
        store
        (root_syms <>
         S.fromList
           [ AsteriusEntitySymbol
             {entityName = "__asterius_jsffi_export_" <> entityName k}
           | k <- export_funcs
           ])
    (result_m, ss_sym_map, func_sym_map, err_msgs, static_mbs) =
      resolveAsteriusModule
        debug
        has_main
        (bundledFFIMarshalState report)
        export_funcs
        merged_m
        (1 .|. functionTag `shiftL` 32)
        (dataTag `shiftL` 32)
