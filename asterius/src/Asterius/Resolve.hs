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
import Asterius.JSFFI
import Asterius.MemoryTrap
import Asterius.Relooper
import Asterius.Tracing
import Asterius.Types
import Asterius.Workarounds
import Control.Exception
import Data.ByteString.Builder
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapM)
import Data.Either
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Traversable
import Foreign
import GHC.Exts (fromList, proxy#)
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)
import System.IO hiding (IO)
import Type.Reflection ((:~~:)(..), TypeRep, eqTypeRep, typeOf, typeRep)

asteriusStaticSize :: AsteriusStatic -> Int
asteriusStaticSize s =
  case s of
    Uninitialized l -> l
    Serialized buf -> SBS.length buf
    _ -> 8

asteriusStaticsSize :: AsteriusStatics -> Int
asteriusStaticsSize ss =
  foldl' (\tot s -> tot + asteriusStaticSize s) 0 (asteriusStatics ss)

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

unresolvedGlobalRegBytes :: UnresolvedGlobalReg -> BinaryenIndex
unresolvedGlobalRegBytes gr =
  case unresolvedGlobalRegType gr of
    I32 -> 4
    F32 -> 4
    _ -> 8

resolveGlobalRegs :: (Monad m, Data a) => a -> m a
resolveGlobalRegs x =
  case eqTypeRep (typeOf x) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case x of
        UnresolvedGetGlobal {..}
          | unresolvedGlobalReg == BaseReg ->
            pure
              Symbol
                { unresolvedSymbol = "MainCapability"
                , symbolOffset = offset_Capability_r
                , resolvedSymbol = Nothing
                }
          | otherwise ->
            pure
              Load
                { signed = False
                , bytes = unresolvedGlobalRegBytes unresolvedGlobalReg
                , offset = 0
                , valueType = unresolvedGlobalRegType unresolvedGlobalReg
                , ptr = gr_ptr unresolvedGlobalReg
                }
        UnresolvedSetGlobal {..}
          | unresolvedGlobalReg == BaseReg ->
            pure $
            emitErrorMessage
              []
              "SetGlobal instruction: attempting to assign to BaseReg"
          | otherwise -> do
            new_value <- resolveGlobalRegs value
            pure $
              Store
                { bytes = unresolvedGlobalRegBytes unresolvedGlobalReg
                , offset = 0
                , ptr = gr_ptr unresolvedGlobalReg
                , value = new_value
                , valueType = unresolvedGlobalRegType unresolvedGlobalReg
                }
        _ -> go
    _ -> go
  where
    gr_ptr gr =
      Unary
        { unaryOp = WrapInt64
        , operand0 =
            Symbol
              { unresolvedSymbol = "MainCapability"
              , symbolOffset = offset_Capability_r + globalRegOffset gr
              , resolvedSymbol = Nothing
              }
        }
    go = gmapM resolveGlobalRegs x

globalRegOffset :: UnresolvedGlobalReg -> Int
globalRegOffset gr =
  case gr of
    VanillaReg 1 -> offset_StgRegTable_rR1
    VanillaReg 2 -> offset_StgRegTable_rR2
    VanillaReg 3 -> offset_StgRegTable_rR3
    VanillaReg 4 -> offset_StgRegTable_rR4
    VanillaReg 5 -> offset_StgRegTable_rR5
    VanillaReg 6 -> offset_StgRegTable_rR6
    VanillaReg 7 -> offset_StgRegTable_rR7
    VanillaReg 8 -> offset_StgRegTable_rR8
    VanillaReg 9 -> offset_StgRegTable_rR9
    VanillaReg 10 -> offset_StgRegTable_rR10
    FloatReg 1 -> offset_StgRegTable_rF1
    FloatReg 2 -> offset_StgRegTable_rF2
    FloatReg 3 -> offset_StgRegTable_rF3
    FloatReg 4 -> offset_StgRegTable_rF4
    FloatReg 5 -> offset_StgRegTable_rF5
    FloatReg 6 -> offset_StgRegTable_rF6
    DoubleReg 1 -> offset_StgRegTable_rD1
    DoubleReg 2 -> offset_StgRegTable_rD2
    DoubleReg 3 -> offset_StgRegTable_rD3
    DoubleReg 4 -> offset_StgRegTable_rD4
    DoubleReg 5 -> offset_StgRegTable_rD5
    DoubleReg 6 -> offset_StgRegTable_rD6
    LongReg 1 -> offset_StgRegTable_rL1
    Sp -> offset_StgRegTable_rSp
    SpLim -> offset_StgRegTable_rSpLim
    Hp -> offset_StgRegTable_rHp
    HpLim -> offset_StgRegTable_rHpLim
    CCCS -> offset_StgRegTable_rCCCS
    CurrentTSO -> offset_StgRegTable_rCurrentTSO
    CurrentNursery -> offset_StgRegTable_rCurrentNursery
    HpAlloc -> offset_StgRegTable_rHpAlloc
    EagerBlackholeInfo -> rf + offset_StgFunTable_stgEagerBlackholeInfo
    GCEnter1 -> rf + offset_StgFunTable_stgGCEnter1
    GCFun -> rf + offset_StgFunTable_stgGCFun
    _ -> throw $ AssignToImmutableGlobalReg gr
  where
    rf = offset_Capability_f - offset_Capability_r

collectAsteriusEntitySymbols :: Data a => a -> S.Set AsteriusEntitySymbol
collectAsteriusEntitySymbols = collect proxy#

data LinkReport = LinkReport
  { childSymbols :: M.Map AsteriusEntitySymbol (S.Set AsteriusEntitySymbol)
  , unfoundSymbols, unavailableSymbols :: S.Set AsteriusEntitySymbol
  , staticsSymbolMap, functionSymbolMap :: M.Map AsteriusEntitySymbol Int64
  , infoTableSet :: S.Set Int64
  , bundledFFIMarshalState :: FFIMarshalState
  } deriving (Show)

instance Semigroup LinkReport where
  r0 <> r1 =
    LinkReport
      { childSymbols = M.unionWith (<>) (childSymbols r0) (childSymbols r1)
      , unfoundSymbols = unfoundSymbols r0 <> unfoundSymbols r1
      , unavailableSymbols = unavailableSymbols r0 <> unavailableSymbols r1
      , staticsSymbolMap = staticsSymbolMap r0 <> staticsSymbolMap r1
      , functionSymbolMap = functionSymbolMap r0 <> functionSymbolMap r1
      , infoTableSet = infoTableSet r0 <> infoTableSet r1
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
      , bundledFFIMarshalState = mempty
      }

mergeSymbols ::
     Monad m
  => Bool
  -> AsteriusStore
  -> S.Set AsteriusEntitySymbol
  -> [AsteriusEntitySymbol]
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
              new_ss <- resolveGlobalRegs ss
              pure $
                Right
                  ( i_staging_sym
                  , mempty {staticsMap = M.fromList [(i_staging_sym, new_ss)]})
            _ ->
              case M.lookup i_staging_sym functionMap of
                Just func -> do
                  new_func <-
                    resolveGlobalRegs func >>=
                    maskUnknownCCallTargets i_staging_sym export_funcs
                  m <-
                    (if debug
                       then addMemoryTrap
                       else pure)
                      mempty
                        {functionMap = M.fromList [(i_staging_sym, new_func)]}
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

makeFunctionTable ::
     AsteriusModule -> (FunctionTable, M.Map AsteriusEntitySymbol Int64)
makeFunctionTable AsteriusModule {..} =
  ( FunctionTable
      {functionNames = map entityName func_syms, tableExportName = "table"}
  , fromList $ zip func_syms [1 .|. functionTag `shiftL` 32 ..])
  where
    func_syms = M.keys functionMap

makeStaticNurseries ::
     Int
  -> Int
  -> M.Map AsteriusEntitySymbol Int64
  -> M.Map AsteriusEntitySymbol AsteriusStatics
  -> (Int, Int, M.Map AsteriusEntitySymbol Int64)
makeStaticNurseries head_mblock head_block init_address_map statics_map =
  (result_mblock, result_block, last_address_map)
  where
    raw_address current_mblock current_block current_offset =
      (mblock_size * current_mblock) + offset_first_block +
      (block_size * current_block) +
      current_offset
    succ_block current_mblock current_block
      | current_block + 1 < blocks_per_mblock =
        (current_mblock, current_block + 1)
      | otherwise = (current_mblock + 1, 0)
    static_allocGroup n current_mblock current_block
      | current_block + n < blocks_per_mblock =
        (current_mblock, current_block, current_mblock, current_block + n)
      | current_block + n == blocks_per_mblock =
        (current_mblock, current_block, current_mblock + 1, 0)
      | n < blocks_per_mblock = (current_mblock + 1, 0, current_mblock + 1, n)
      | otherwise = (current_mblock + 1, 0, current_block + 1 + m, 0)
      where
        raw_blocks_per_mblock = mblock_size `quot` block_size
        m =
          1 +
          (((n - blocks_per_mblock) `roundup` raw_blocks_per_mblock) `quot`
           raw_blocks_per_mblock)
    static_allocate raw_size current_mblock current_block current_free
      | current_free + size < block_size =
        ( current_mblock
        , current_block
        , current_free
        , current_mblock
        , current_block
        , current_free + size)
      | current_free + size == block_size =
        ( current_mblock
        , current_block
        , current_free
        , next_mblock
        , next_block
        , 0)
      | size < block_size =
        (next_mblock, next_block, 0, next_mblock, next_block, size)
      | otherwise =
        (big_mblock, big_block, 0, big_next_mblock, big_next_block, 0)
      where
        size = raw_size `roundup` 16
        blocks = raw_size `roundup` block_size
        (next_mblock, next_block) = succ_block current_mblock current_block
        (big_mblock, big_block, big_next_mblock, big_next_block) =
          static_allocGroup blocks next_mblock next_block
    layout_section (current_mblock, current_block, current_free, address_map) sym sec =
      ( next_mblock
      , next_block
      , next_free
      , M.insert
          sym
          (fromIntegral (raw_address sec_mblock sec_block sec_offset))
          address_map)
      where
        (sec_mblock, sec_block, sec_offset, next_mblock, next_block, next_free) =
          static_allocate
            (asteriusStaticsSize sec)
            current_mblock
            current_block
            current_free
    (last_mblock, last_block, last_free, last_address_map) =
      M.foldlWithKey'
        layout_section
        (head_mblock, head_block, 0, init_address_map)
        statics_map
    (result_mblock, result_block) =
      case last_free of
        0 -> (last_mblock, last_block)
        _ -> succ_block last_mblock last_block

makeStaticsOffsetTable ::
     AsteriusModule -> (Int64, M.Map AsteriusEntitySymbol Int64)
makeStaticsOffsetTable AsteriusModule {..} = (last_o, closures_address_map)
  where
    (closures, non_closures) =
      M.partition ((== Closure) . staticsType) staticsMap
    (closures_head_mblock, closures_head_block, non_closures_address_map) =
      makeStaticNurseries
        ((fromIntegral dataTag `shiftL` 32) `quot` mblock_size)
        0
        M.empty
        non_closures
    (closures_result_mblock, closures_result_block, closures_address_map) =
      makeStaticNurseries
        closures_head_mblock
        closures_head_block
        non_closures_address_map
        closures
    last_address
      | closures_result_block == 0 = mblock_size * closures_result_mblock
      | otherwise =
        (mblock_size * closures_result_mblock) + offset_first_block +
        (block_size * closures_result_block)
    last_o = fromIntegral $ last_address .&. 0xFFFFFFFF

makeInfoTableSet ::
     AsteriusModule -> M.Map AsteriusEntitySymbol Int64 -> S.Set Int64
makeInfoTableSet AsteriusModule {..} sym_map =
  S.map (sym_map !) $
  M.keysSet $ M.filter ((== InfoTable) . staticsType) staticsMap

makeMemory ::
     AsteriusModule -> Int64 -> M.Map AsteriusEntitySymbol Int64 -> Memory
makeMemory AsteriusModule {..} last_o sym_map =
  Memory
    { initialPages =
        fromIntegral $
        roundup (fromIntegral last_o) mblock_size `div` wasmPageSize
    , memoryExportName = "memory"
    , dataSegments = combined_segs
    }
  where
    uncombined_segs =
      M.foldlWithKey'
        (\segs sym AsteriusStatics {..} ->
           snd $
           foldl'
             (\(p, inner_segs) seg ->
                ( p + fromIntegral (asteriusStaticSize seg)
                , case seg of
                    SymbolStatic {} ->
                      error
                        "Asterius.Resolve.makeMemory: unresolved SymbolStatic!"
                    Serialized buf ->
                      DataSegment
                        { content = buf
                        , offset = fromIntegral $ p .&. 0xFFFFFFFF
                        } :
                      inner_segs
                    Uninitialized {} -> inner_segs))
             (sym_map ! sym, segs)
             asteriusStatics)
        []
        staticsMap
    combined_segs =
      foldr
        (\seg@DataSegment {content = seg_content, offset = seg_o} stack ->
           case stack of
             DataSegment {content = stack_top_content, offset = stack_top_o}:stack_rest
               | fromIntegral seg_o + SBS.length seg_content ==
                   fromIntegral stack_top_o ->
                 DataSegment
                   {content = seg_content <> stack_top_content, offset = seg_o} :
                 stack_rest
               | fromIntegral seg_o + SBS.length seg_content >
                   fromIntegral stack_top_o ->
                 error "Asterius.Resolve.makeMemory: overlapping sections!"
             _ -> seg : stack)
        []
        (sortOn (\DataSegment {offset = o} -> o) uncombined_segs)

resolveEntitySymbols ::
     (Monad m, Data a)
  => M.Map AsteriusEntitySymbol Int64
  -> M.Map AsteriusEntitySymbol Int64
  -> a
  -> m a
resolveEntitySymbols ss_sym_map func_sym_map = f
  where
    f :: (Monad m, Data a) => a -> m a
    f t =
      case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
        Just HRefl ->
          case t of
            Symbol {..} ->
              pure $
              case ( M.lookup unresolvedSymbol ss_sym_map
                   , M.lookup unresolvedSymbol func_sym_map) of
                (Just r, _) -> t {resolvedSymbol = Just r}
                (_, Just r) -> t {resolvedSymbol = Just r}
                _ ->
                  emitErrorMessage [I64] $
                  "Unresolved symbol: " <> entityName unresolvedSymbol
            _ -> go
        _ ->
          case eqTypeRep (typeOf t) (typeRep :: TypeRep AsteriusStatic) of
            Just HRefl ->
              case t of
                SymbolStatic unresolvedSymbol symbolOffset ->
                  pure $
                  case ( M.lookup unresolvedSymbol ss_sym_map
                       , M.lookup unresolvedSymbol func_sym_map) of
                    (Just r, _) ->
                      Serialized
                        (encodeStorable (r + fromIntegral symbolOffset))
                    (_, Just r) ->
                      Serialized
                        (encodeStorable (r + fromIntegral symbolOffset))
                    _ -> t
                _ -> pure t
            _ -> go
      where
        go = gmapM f t

collectEvents :: Data a => a -> S.Set Event
collectEvents = collect proxy#

rewriteEmitEvent :: (Monad m, Data a) => a -> m (a, [Event])
rewriteEmitEvent x = do
  new_x <- f x
  pure (new_x, msg_lst)
  where
    msg_lst = S.toList $ collectEvents x
    msg_tbl = M.fromList $ zip msg_lst [(0 :: Int32) ..]
    msg_lookup = (msg_tbl !)
    f :: (Monad m, Data a) => a -> m a
    f t =
      case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
        Just HRefl ->
          case t of
            EmitEvent {..} ->
              pure
                CallImport
                  { target' = "__asterius_eventI32"
                  , operands = [ConstI32 $ msg_lookup event]
                  , callImportReturnTypes = []
                  }
            _ -> go
        _ -> go
      where
        go = gmapM f t

resolveAsteriusModule ::
     Monad m
  => Bool
  -> FFIMarshalState
  -> [AsteriusEntitySymbol]
  -> AsteriusModule
  -> m ( Module
       , M.Map AsteriusEntitySymbol Int64
       , M.Map AsteriusEntitySymbol Int64
       , [Event])
resolveAsteriusModule debug bundled_ffi_state export_funcs m_globals_resolved = do
  let (func_table, func_sym_map) = makeFunctionTable m_globals_resolved
      (last_o, ss_sym_map) = makeStaticsOffsetTable m_globals_resolved
      resolve_syms :: (Monad m, Data a) => a -> m a
      resolve_syms = resolveEntitySymbols ss_sym_map func_sym_map
  m_globals_syms_resolved <- resolve_syms m_globals_resolved
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
      new_func <-
        (if debug
           then addTracingModule func_sym_map func_sym functionType func
           else pure func) >>=
        relooperDeep
      pure (entityName func_sym, new_func)
  (new_mod, err_msgs) <-
    rewriteEmitEvent
      Module
        { functionMap' = new_function_map
        , functionImports = func_imports
        , functionExports =
            rtsAsteriusFunctionExports debug <>
            [ FunctionExport
              {internalName = "__asterius_jsffi_export_" <> k, externalName = k}
            | k <- map entityName export_funcs
            ]
        , functionTable = func_table
        , memory = makeMemory m_globals_syms_resolved last_o ss_sym_map
        }
  pure (new_mod, ss_sym_map, func_sym_map, err_msgs)

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
      export_funcs
  (result_m, ss_sym_map, func_sym_map, err_msgs) <-
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
