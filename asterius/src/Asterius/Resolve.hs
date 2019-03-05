{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Resolve
  ( unresolvedGlobalRegType
  , LinkReport(..)
  , linkStart
  , writeDot
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
import Data.ByteString.Builder
import Data.Data (Data)
import Data.Foldable
import Data.List
import qualified Data.Map.Lazy as LM
import qualified Data.Set as S
import Foreign
import GHC.Generics
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)
import System.IO hiding (IO)
import Unsafe.Coerce

unresolvedGlobalRegType :: UnresolvedGlobalReg -> ValueType
unresolvedGlobalRegType gr =
  case gr of
    FloatReg _ -> F32
    DoubleReg _ -> F64
    _ -> I64

collectAsteriusEntitySymbols :: Data a => a -> S.Set AsteriusEntitySymbol
collectAsteriusEntitySymbols = collect

data LinkReport = LinkReport
  { childSymbols :: LM.Map AsteriusEntitySymbol (S.Set AsteriusEntitySymbol)
  , unavailableSymbols :: S.Set AsteriusEntitySymbol
  , staticsSymbolMap, functionSymbolMap :: LM.Map AsteriusEntitySymbol Int64
  , infoTableSet :: S.Set Int64
  , staticMBlocks :: Int
  , bundledFFIMarshalState :: FFIMarshalState
  } deriving (Generic, Show)

instance Binary LinkReport

instance Semigroup LinkReport where
  r0 <> r1 =
    LinkReport
      { childSymbols = LM.unionWith (<>) (childSymbols r0) (childSymbols r1)
      , unavailableSymbols = unavailableSymbols r0 <> unavailableSymbols r1
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
      { childSymbols = mempty
      , unavailableSymbols = mempty
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
mergeSymbols debug AsteriusModule {..} root_syms =
  (final_m, final_rep {bundledFFIMarshalState = ffiMarshalState})
  where
    (_, final_rep, final_m) = go (root_syms, mempty, mempty)
    go i@(i_staging_syms, _, _)
      | S.null i_staging_syms = i
      | otherwise = go $ iter i
    iter (i_staging_syms, i_rep, i_m) =
      let (i_unavailable_syms, i_sym_modlets) =
            S.foldr'
              (\i_staging_sym (i_unavailable_syms_acc, i_sym_modlets_acc) ->
                 case LM.lookup i_staging_sym staticsMap of
                   Just ss ->
                     ( i_unavailable_syms_acc
                     , ( i_staging_sym
                       , mempty {staticsMap = LM.singleton i_staging_sym ss}) :
                       i_sym_modlets_acc)
                   _ ->
                     case LM.lookup i_staging_sym functionMap of
                       Just func ->
                         ( i_unavailable_syms_acc
                         , ( i_staging_sym
                           , mempty
                               {functionMap = LM.singleton i_staging_sym func}) :
                           i_sym_modlets_acc)
                       _
                         | LM.member i_staging_sym functionErrorMap ->
                           ( i_unavailable_syms_acc
                           , ( i_staging_sym
                             , mempty
                                 { functionMap =
                                     LM.singleton
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
                                               (functionErrorMap ! i_staging_sym)
                                         }
                                 }) :
                             i_sym_modlets_acc)
                         | otherwise ->
                           ( i_staging_sym : i_unavailable_syms_acc
                           , i_sym_modlets_acc))
              ([], [])
              i_staging_syms
          i_child_map =
            LM.fromList
              [ ( i_staging_sym
                , S.filter (/= i_staging_sym) $
                  collectAsteriusEntitySymbols i_modlet)
              | (i_staging_sym, i_modlet) <- i_sym_modlets
              ]
          o_rep =
            mempty
              { childSymbols = i_child_map
              , unavailableSymbols = S.fromList i_unavailable_syms
              } <>
            i_rep
          o_m = mconcat (map snd i_sym_modlets) <> i_m
          o_staging_syms =
            mconcat (LM.elems $ childSymbols o_rep) `S.difference`
            S.unions
              [ unavailableSymbols o_rep
              , S.fromList $ LM.keys $ childSymbols o_rep
              ]
       in (o_staging_syms, o_rep, o_m)

makeInfoTableSet ::
     AsteriusModule -> LM.Map AsteriusEntitySymbol Int64 -> S.Set Int64
makeInfoTableSet AsteriusModule {..} sym_map =
  S.map (sym_map !) $
  LM.keysSet $ LM.filter ((== InfoTable) . staticsType) staticsMap

resolveAsteriusModule ::
     Monad m
  => Bool
  -> FFIMarshalState
  -> [AsteriusEntitySymbol]
  -> AsteriusModule
  -> m ( Module
       , LM.Map AsteriusEntitySymbol Int64
       , LM.Map AsteriusEntitySymbol Int64
       , [Event]
       , Int)
resolveAsteriusModule debug bundled_ffi_state export_funcs m_globals_resolved = do
  let (func_sym_map, _) =
        makeFunctionSymbolTable
          m_globals_resolved
          (1 .|. functionTag `shiftL` 32)
      func_table = makeFunctionTable func_sym_map
      (ss_sym_map, last_addr) =
        makeDataSymbolTable m_globals_resolved (dataTag `shiftL` 32)
      all_sym_map = func_sym_map <> ss_sym_map
  let func_imports =
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
      mem = makeMemory m_globals_resolved all_sym_map last_addr
      new_mod =
        Module
          { functionMap' = new_function_map
          , functionImports = func_imports
          , functionExports =
              rtsAsteriusFunctionExports debug <>
              [ FunctionExport
                { internalName = "__asterius_jsffi_export_" <> k
                , externalName = k
                }
              | k <- map entityName export_funcs
              ]
          , functionTable = func_table
          , memory = mem
          }
      err_msgs = eventTable all_event_map
  pure
    ( new_mod
    , ss_sym_map
    , func_sym_map
    , err_msgs
    , fromIntegral (initialPages mem) `quot` (mblock_size `quot` wasmPageSize))

linkStart ::
     Monad m
  => Bool
  -> AsteriusModule
  -> S.Set AsteriusEntitySymbol
  -> [AsteriusEntitySymbol]
  -> m (Module, [Event], LinkReport)
linkStart debug store root_syms export_funcs = do
  let (merged_m, report) =
        mergeSymbols
          debug
          store
          (root_syms <>
           S.fromList
             [ AsteriusEntitySymbol
               {entityName = "__asterius_jsffi_export_" <> entityName k}
             | k <- export_funcs
             ])
  (result_m, ss_sym_map, func_sym_map, err_msgs, static_mbs) <-
    resolveAsteriusModule
      debug
      (bundledFFIMarshalState report)
      export_funcs
      merged_m
  pure
    ( result_m
    , err_msgs
    , report
        { staticsSymbolMap = ss_sym_map
        , functionSymbolMap = func_sym_map
        , infoTableSet = makeInfoTableSet merged_m ss_sym_map
        , staticMBlocks = static_mbs
        })

renderDot :: LinkReport -> Builder
renderDot LinkReport {..} =
  mconcat $
  ["digraph {\n"] <>
  concat
    [ ["    ", sym unavailable_sym, " [color=red];\n"]
    | unavailable_sym <- toList unavailableSymbols
    ] <>
  concat
    [ ["    ", sym u, " -> ", sym v, ";\n"]
    | (u, vs) <- LM.toList childSymbols
    , v <- toList vs
    ] <>
  ["}\n"]
  where
    sym = shortByteString . entityName

writeDot :: FilePath -> LinkReport -> IO ()
writeDot p r = do
  h <- openBinaryFile p WriteMode
  hSetBuffering h $ BlockBuffering Nothing
  hPutBuilder h $ renderDot r
  hClose h
