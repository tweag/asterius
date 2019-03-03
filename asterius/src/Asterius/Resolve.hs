{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
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
import Asterius.Internals.SYB
import Asterius.JSFFI
import Asterius.MemoryTrap
import Asterius.Passes.All
import Asterius.Passes.Common
import Asterius.Passes.DataSymbolTable
import Asterius.Passes.Events
import Asterius.Passes.FunctionSymbolTable
import Asterius.Passes.GlobalRegs
import Asterius.Passes.ResolveSymbols
import Asterius.Types
import Asterius.Workarounds
import Control.Monad.State.Strict
import Data.Binary
import Data.ByteString.Builder
import Data.Data (Data, gmapM)
import Data.Either
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Traversable
import Foreign
import GHC.Exts (fromList, proxy#)
import GHC.Generics
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)
import System.IO hiding (IO)
import Type.Reflection ((:~~:)(..), TypeRep, eqTypeRep, typeOf, typeRep)

unresolvedLocalRegType :: UnresolvedLocalReg -> ValueType
unresolvedLocalRegType lr =
  case lr of
    UniqueLocalReg _ vt -> vt
    QuotRemI32X -> I32
    QuotRemI32Y -> I32
    QuotRemI64X -> I64
    QuotRemI64Y -> I64

collectUnresolvedLocalRegs :: Data a => a -> S.Set UnresolvedLocalReg
collectUnresolvedLocalRegs = collect proxy#

resolveLocalRegs :: (Monad m, Data a) => Int -> a -> m (a, [ValueType])
resolveLocalRegs func_param_n t = do
  new_t <- f t
  pure (new_t, I32 : I32 : I64 : [unresolvedLocalRegType lr | (lr, _) <- lrs])
  where
    lrs =
      zip
        (toList $ collectUnresolvedLocalRegs t)
        ([fromIntegral func_param_n + 3 ..] :: [BinaryenIndex])
    lr_map = fromList lrs
    lr_idx = (lr_map !)
    f :: (Monad m, Data a) => a -> m a
    f x =
      case eqTypeRep (typeOf x) (typeRep :: TypeRep Expression) of
        Just HRefl ->
          case x of
            UnresolvedGetLocal {..} ->
              pure
                GetLocal
                  { index = lr_idx unresolvedLocalReg
                  , valueType = unresolvedLocalRegType unresolvedLocalReg
                  }
            UnresolvedSetLocal {..} -> do
              new_value <- f value
              pure
                SetLocal {index = lr_idx unresolvedLocalReg, value = new_value}
            _ -> go
        _ -> go
      where
        go = gmapM f x

unresolvedGlobalRegType :: UnresolvedGlobalReg -> ValueType
unresolvedGlobalRegType gr =
  case gr of
    FloatReg _ -> F32
    DoubleReg _ -> F64
    _ -> I64

collectAsteriusEntitySymbols :: Data a => a -> S.Set AsteriusEntitySymbol
collectAsteriusEntitySymbols = collect proxy#

data LinkReport = LinkReport
  { childSymbols :: M.Map AsteriusEntitySymbol (S.Set AsteriusEntitySymbol)
  , unfoundSymbols, unavailableSymbols :: S.Set AsteriusEntitySymbol
  , staticsSymbolMap, functionSymbolMap :: M.Map AsteriusEntitySymbol Int64
  , infoTableSet :: S.Set Int64
  , staticMBlocks :: Int
  , bundledFFIMarshalState :: FFIMarshalState
  } deriving (Generic, Show)

instance Binary LinkReport

instance Semigroup LinkReport where
  r0 <> r1 =
    LinkReport
      { childSymbols = M.unionWith (<>) (childSymbols r0) (childSymbols r1)
      , unfoundSymbols = unfoundSymbols r0 <> unfoundSymbols r1
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
      , unfoundSymbols = mempty
      , unavailableSymbols = mempty
      , staticsSymbolMap = mempty
      , functionSymbolMap = mempty
      , infoTableSet = mempty
      , staticMBlocks = 0
      , bundledFFIMarshalState = mempty
      }

mergeSymbols ::
     Monad m
  => Bool
  -> AsteriusStore
  -> S.Set AsteriusEntitySymbol
  -> S.Set AsteriusEntitySymbol
  -> m (AsteriusModule, LinkReport)
mergeSymbols debug AsteriusStore {..} root_syms export_funcs = do
  (_, final_rep, final_m) <- go (root_syms, mempty, mempty)
  pure (final_m, final_rep)
  where
    go i@(i_staging_syms, _, _) = do
      o <- iter i
      if S.null i_staging_syms
        then pure o
        else go o
    iter (i_staging_syms, i_rep, i_m) = do
      let (i_unfound_syms, i_sym_mods) =
            partitionEithers
              [ case M.lookup i_staging_sym symbolMap of
                Just mod_sym -> Right (i_staging_sym, moduleMap ! mod_sym)
                _ -> Left i_staging_sym
              | i_staging_sym <- S.toList i_staging_syms
              ]
      (i_unavailable_syms, i_sym_modlets) <-
        fmap partitionEithers $
        for i_sym_mods $ \(i_staging_sym, AsteriusModule {..}) ->
          case M.lookup i_staging_sym staticsMap of
            Just ss -> do
              new_ss <- everywhereM resolveGlobalRegs ss
              pure $
                Right
                  ( i_staging_sym
                  , mempty {staticsMap = M.fromList [(i_staging_sym, new_ss)]})
            _ ->
              case M.lookup i_staging_sym functionMap of
                Just func -> do
                  new_func <-
                    everywhereM resolveGlobalRegs func >>=
                    maskUnknownCCallTargets i_staging_sym export_funcs
                  m <-
                    (if debug
                       then addMemoryTrap
                       else pure)
                      mempty {functionMap = M.singleton i_staging_sym new_func}
                  pure $ Right (i_staging_sym, m)
                _
                  | M.member i_staging_sym functionErrorMap ->
                    pure $
                    Right
                      ( i_staging_sym
                      , mempty
                          { functionMap =
                              M.fromList
                                [ ( i_staging_sym
                                  , AsteriusFunction
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
                                      })
                                ]
                          })
                  | otherwise -> pure $ Left i_staging_sym
      let i_child_map =
            M.fromList
              [ ( i_staging_sym
                , S.filter (/= i_staging_sym) $
                  collectAsteriusEntitySymbols i_modlet)
              | (i_staging_sym, i_modlet) <- i_sym_modlets
              ]
          o_rep =
            mempty
              { childSymbols = i_child_map
              , unfoundSymbols = fromList i_unfound_syms
              , unavailableSymbols = fromList i_unavailable_syms
              , bundledFFIMarshalState =
                  mconcat
                    [ffiMarshalState | (_, AsteriusModule {..}) <- i_sym_mods]
              } <>
            i_rep
          o_m = mconcat (map snd i_sym_modlets) <> i_m
          o_staging_syms =
            mconcat (M.elems $ childSymbols o_rep) `S.difference`
            S.unions
              [ unfoundSymbols o_rep
              , unavailableSymbols o_rep
              , fromList $ M.keys $ childSymbols o_rep
              ]
      pure (o_staging_syms, o_rep, o_m)

makeInfoTableSet ::
     AsteriusModule -> M.Map AsteriusEntitySymbol Int64 -> S.Set Int64
makeInfoTableSet AsteriusModule {..} sym_map =
  S.map (sym_map !) $
  M.keysSet $ M.filter ((== InfoTable) . staticsType) staticsMap

resolveAsteriusModule ::
     Monad m
  => Bool
  -> FFIMarshalState
  -> [AsteriusEntitySymbol]
  -> AsteriusModule
  -> m ( Module
       , M.Map AsteriusEntitySymbol Int64
       , M.Map AsteriusEntitySymbol Int64
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
  m_globals_syms_resolved <-
    everywhereM (resolveSymbols all_sym_map) m_globals_resolved
  let func_imports =
        rtsFunctionImports debug <> generateFFIFunctionImports bundled_ffi_state
  new_function_map <-
    fmap M.fromList $
    for (M.toList $ functionMap m_globals_syms_resolved) $ \(func_sym, AsteriusFunction {..}) -> do
      (body_locals_resolved, local_types) <-
        resolveLocalRegs (length $ paramTypes functionType) body
      let func =
            Function
              { functionType = functionType
              , varTypes = local_types
              , body = body_locals_resolved
              }
      {-new_func <-
        (if debug
           then addTracingModule func_sym_map func_sym functionType func
           else pure func) >>=
        relooperDeep-}
      let new_func = allPasses debug func
      pure (entityName func_sym, new_func)
  let mem = makeMemory m_globals_syms_resolved all_sym_map last_addr
      (new_mod, ps) =
        runState
          (everywhereM
             rewriteEmitEvent
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
               })
          defaultPassesState
      err_msgs = eventTable ps
  pure
    ( new_mod
    , ss_sym_map
    , func_sym_map
    , err_msgs
    , fromIntegral (initialPages mem) `quot` (mblock_size `quot` wasmPageSize))

linkStart ::
     Monad m
  => Bool
  -> AsteriusStore
  -> S.Set AsteriusEntitySymbol
  -> [AsteriusEntitySymbol]
  -> m (Module, [Event], LinkReport)
linkStart debug store root_syms export_funcs = do
  (merged_m, report) <-
    mergeSymbols
      debug
      store
      (root_syms <>
       S.fromList
         [ AsteriusEntitySymbol
           {entityName = "__asterius_jsffi_export_" <> entityName k}
         | k <- export_funcs
         ])
      (S.fromList export_funcs)
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
    [ ["    ", sym unfound_sym, " [color=orange];\n"]
    | unfound_sym <- toList unfoundSymbols
    ] <>
  concat
    [ ["    ", sym unavailable_sym, " [color=red];\n"]
    | unavailable_sym <- toList unavailableSymbols
    ] <>
  concat
    [ ["    ", sym u, " -> ", sym v, ";\n"]
    | (u, vs) <- M.toList childSymbols
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
