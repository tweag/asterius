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
import Asterius.JSFFI
import Asterius.MemoryTrap
import Asterius.Relooper
import Asterius.Tracing
import Asterius.Types
import Asterius.Workarounds
import Control.Exception
import Data.ByteString.Builder
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapT)
import Data.Either
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

resolveLocalRegs :: Data a => Int -> a -> (a, [ValueType])
resolveLocalRegs func_param_n t =
  (f t, I32 : I32 : I64 : [unresolvedLocalRegType lr | (lr, _) <- lrs])
  where
    lrs =
      zip
        (toList $ collectUnresolvedLocalRegs t)
        ([fromIntegral func_param_n + 3 ..] :: [BinaryenIndex])
    lr_map = fromList lrs
    lr_idx = (lr_map !)
    f :: Data a => a -> a
    f x =
      case eqTypeRep (typeOf x) (typeRep :: TypeRep Expression) of
        Just HRefl ->
          case x of
            UnresolvedGetLocal {..} ->
              GetLocal
                { index = lr_idx unresolvedLocalReg
                , valueType = unresolvedLocalRegType unresolvedLocalReg
                }
            UnresolvedSetLocal {..} ->
              SetLocal {index = lr_idx unresolvedLocalReg, value = f value}
            _ -> go
        _ -> go
      where
        go = gmapT f x

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

resolveGlobalRegs :: Data a => a -> a
resolveGlobalRegs x =
  case eqTypeRep (typeOf x) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case x of
        UnresolvedGetGlobal {..}
          | unresolvedGlobalReg == BaseReg ->
            Binary
              { binaryOp = AddInt64
              , operand0 = Unresolved {unresolvedSymbol = "MainCapability"}
              , operand1 = ConstI64 $ fromIntegral offset_Capability_r
              }
          | otherwise ->
            Load
              { signed = False
              , bytes = unresolvedGlobalRegBytes unresolvedGlobalReg
              , offset = 0
              , align = 0
              , valueType = unresolvedGlobalRegType unresolvedGlobalReg
              , ptr = gr_ptr unresolvedGlobalReg
              }
        UnresolvedSetGlobal {..}
          | unresolvedGlobalReg == BaseReg ->
            marshalErrorCode errSetBaseReg None
          | otherwise ->
            Store
              { bytes = unresolvedGlobalRegBytes unresolvedGlobalReg
              , offset = 0
              , align = 0
              , ptr = gr_ptr unresolvedGlobalReg
              , value = resolveGlobalRegs value
              , valueType = unresolvedGlobalRegType unresolvedGlobalReg
              }
        _ -> go
    _ -> go
  where
    gr_ptr gr =
      Unary
        { unaryOp = WrapInt64
        , operand0 =
            Binary
              { binaryOp = AddInt64
              , operand0 = Unresolved {unresolvedSymbol = "MainCapability"}
              , operand1 =
                  ConstI64 $
                  fromIntegral $ offset_Capability_r + globalRegOffset gr
              }
        }
    go = gmapT resolveGlobalRegs x

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
    _ -> throw $ AssignToImmutableGlobalReg gr

collectAsteriusEntitySymbols :: Data a => a -> S.Set AsteriusEntitySymbol
collectAsteriusEntitySymbols = collect proxy#

data LinkReport = LinkReport
  { childSymbols :: M.Map AsteriusEntitySymbol (S.Set AsteriusEntitySymbol)
  , unfoundSymbols, unavailableSymbols :: S.Set AsteriusEntitySymbol
  , staticsSymbolMap, functionSymbolMap :: M.Map AsteriusEntitySymbol Int64
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
      , bundledFFIMarshalState = mempty
      }

mergeSymbols ::
     Bool
  -> AsteriusStore
  -> S.Set AsteriusEntitySymbol
  -> [AsteriusEntitySymbol]
  -> (Maybe AsteriusModule, LinkReport)
mergeSymbols debug AsteriusStore {..} root_syms export_funcs =
  (maybe_final_m, final_rep)
  where
    maybe_final_m
      | S.null (unfoundSymbols final_rep) &&
          S.null (unavailableSymbols final_rep) = Just final_m
      | otherwise = Nothing
    (_, final_rep, final_m) = go (root_syms, mempty, mempty)
    go i@(i_staging_syms, _, _)
      | S.null i_staging_syms = o
      | otherwise = go o
      where
        o = iter i
    iter (i_staging_syms, i_rep, i_m) = (o_staging_syms, o_rep, o_m)
      where
        (i_unfound_syms, i_sym_mods) =
          partitionEithers
            [ case M.lookup i_staging_sym symbolMap of
              Just mod_sym -> Right (i_staging_sym, moduleMap ! mod_sym)
              _ -> Left i_staging_sym
            | i_staging_sym <- toList i_staging_syms
            ]
        (i_unavailable_syms, i_sym_modlets) =
          partitionEithers
            [ case M.lookup i_staging_sym staticsMap of
              Just ss ->
                Right
                  ( i_staging_sym
                  , mempty
                      { staticsMap =
                          M.fromList [(i_staging_sym, resolveGlobalRegs ss)]
                      })
              _ ->
                case M.lookup i_staging_sym functionMap of
                  Just func ->
                    Right
                      ( i_staging_sym
                      , let m0 =
                              mempty
                                { functionMap =
                                    M.fromList
                                      [ ( i_staging_sym
                                        , patchWritePtrArrayOp $
                                          maskUnknownCCallTargets export_funcs $
                                          resolveGlobalRegs func)
                                      ]
                                }
                         in if debug
                              then addMemoryTrap m0
                              else m0)
                  _
                    | M.member i_staging_sym functionErrorMap ->
                      Right
                        ( i_staging_sym
                        , mempty
                            { functionMap =
                                M.fromList
                                  [ ( i_staging_sym
                                    , AsteriusFunction
                                        { functionType =
                                            FunctionType
                                              { returnType = I64
                                              , paramTypes = []
                                              }
                                        , body =
                                            marshalErrorCode
                                              errBrokenFunction
                                              I64
                                        })
                                  ]
                            })
                    | otherwise -> Left i_staging_sym
            | (i_staging_sym, AsteriusModule {..}) <- i_sym_mods
            ]
        i_child_map =
          fromList
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

makeFunctionTable ::
     AsteriusModule -> (FunctionTable, M.Map AsteriusEntitySymbol Int64)
makeFunctionTable AsteriusModule {..} =
  ( FunctionTable {functionNames = map entityName func_syms}
  , fromList $ zip func_syms [1 ..])
  where
    func_syms = M.keys functionMap

makeStaticsOffsetTable ::
     AsteriusModule -> (Int64, M.Map AsteriusEntitySymbol Int64)
makeStaticsOffsetTable AsteriusModule {..} = (last_o, fromList statics_map)
  where
    (last_o, statics_map) = layoutStatics $ M.toList staticsMap
    layoutStatics = foldl' iterLayoutStaticsState (16, [])
    iterLayoutStaticsState (ptr, sym_map) (ss_sym, ss) =
      ( fromIntegral $ roundup (fromIntegral ptr + asteriusStaticsSize ss) 16
      , (ss_sym, ptr) : sym_map)

makeMemory ::
     Bool
  -> AsteriusModule
  -> Int64
  -> M.Map AsteriusEntitySymbol Int64
  -> Memory
makeMemory debug AsteriusModule {..} last_o sym_map =
  Memory
    { initialPages =
        fromIntegral $
        roundup (fromIntegral last_o) mblock_size `div` wasmPageSize
    , maximumPages = 65535
    , exportName = "mem"
    , dataSegments =
        if debug
          then uncombined_segs
          else combine uncombined_segs
    }
  where
    uncombined_segs = concatMap data_segs $ M.toList staticsMap
    data_segs (ss_sym, AsteriusStatics {..}) =
      snd $
      foldl'
        (\(p, segs) s ->
           ( p + fromIntegral (asteriusStaticSize s)
           , case s of
               Serialized buf ->
                 DataSegment {content = buf, offset = ConstI32 $ fromIntegral p} :
                 segs
               Uninitialized {} -> segs
               _ ->
                 error $
                 "Encountered unresolved content " <> show s <> " in makeMemory"))
        (sym_map ! ss_sym, [])
        asteriusStatics
    combine segs =
      reverse $
      foldl'
        (\stack seg@DataSegment {content = seg_content, offset = ConstI32 seg_o} ->
           case stack of
             DataSegment { content = stack_top_content
                         , offset = ConstI32 stack_top_o
                         }:stack_rest
               | fromIntegral stack_top_o + SBS.length stack_top_content ==
                   fromIntegral seg_o ->
                 DataSegment
                   { content = stack_top_content <> seg_content
                   , offset = ConstI32 stack_top_o
                   } :
                 stack_rest
             _ -> seg : stack)
        []
        (sortOn (\DataSegment {offset = ConstI32 o} -> o) segs)

resolveEntitySymbols :: Data a => M.Map AsteriusEntitySymbol Int64 -> a -> a
resolveEntitySymbols sym_table = f
  where
    f :: Data a => a -> a
    f t =
      case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
        Just HRefl ->
          case t of
            Unresolved {..} -> ConstI64 $ subst unresolvedSymbol
            UnresolvedOff {..} ->
              ConstI64 $ subst unresolvedSymbol + fromIntegral offset'
            _ -> go
        _ ->
          case eqTypeRep (typeOf t) (typeRep :: TypeRep AsteriusStatic) of
            Just HRefl ->
              case t of
                UnresolvedStatic unresolvedSymbol ->
                  Serialized (encodeStorable (subst unresolvedSymbol))
                UnresolvedOffStatic unresolvedSymbol offset' ->
                  Serialized
                    (encodeStorable
                       (subst unresolvedSymbol + fromIntegral offset'))
                _ -> t
            _ -> go
      where
        go = gmapT f t
        subst = (sym_table !)

resolveFunctionImport :: AsteriusFunctionImport -> FunctionImport
resolveFunctionImport AsteriusFunctionImport {..} =
  FunctionImport
    { internalName = internalName
    , externalModuleName = externalModuleName
    , externalBaseName = externalBaseName
    , functionTypeName = generateWasmFunctionTypeName functionType
    }

resolveAsteriusModule ::
     Bool
  -> FFIMarshalState
  -> [AsteriusEntitySymbol]
  -> AsteriusModule
  -> ( Module
     , M.Map AsteriusEntitySymbol Int64
     , M.Map AsteriusEntitySymbol Int64)
resolveAsteriusModule debug bundled_ffi_state export_funcs m_globals_resolved =
  ( Module
      { functionTypeMap =
          M.fromList
            [ (generateWasmFunctionTypeName ft, ft)
            | ft <-
                [functionType | AsteriusFunctionImport {..} <- func_imports] <>
                [ functionType
                | AsteriusFunction {..} <-
                    M.elems $ functionMap m_globals_syms_resolved
                ]
            ]
      , functionMap' =
          M.fromList
            [ ( entityName func_sym
              , relooperDeep $
                if debug
                  then addTracingModule func_sym_map func_sym functionType func
                  else func)
            | (func_sym, AsteriusFunction {..}) <-
                M.toList $ functionMap m_globals_syms_resolved
            , let (body_locals_resolved, local_types) =
                    resolveLocalRegs (length $ paramTypes functionType) body
                  func =
                    Function
                      { functionTypeName =
                          generateWasmFunctionTypeName functionType
                      , varTypes = local_types
                      , body = body_locals_resolved
                      }
            ]
      , functionImports = [resolveFunctionImport imp | imp <- func_imports]
      , tableImports = []
      , globalImports = []
      , functionExports =
          rtsAsteriusFunctionExports debug <>
          [ FunctionExport
            {internalName = "__asterius_jsffi_export_" <> k, externalName = k}
          | k <- map entityName export_funcs
          ]
      , tableExports = []
      , globalExports = []
      , globalMap = M.fromList []
      , functionTable = Just func_table
      , memory =
          Just $ makeMemory debug m_globals_syms_resolved last_o ss_sym_map
      , startFunctionName = Nothing
      }
  , ss_sym_map
  , func_sym_map)
  where
    (func_table, func_sym_map) = makeFunctionTable m_globals_resolved
    (last_o, ss_sym_map) = makeStaticsOffsetTable m_globals_resolved
    resolve_syms :: Data a => a -> a
    resolve_syms = resolveEntitySymbols $ func_sym_map <> ss_sym_map
    m_globals_syms_resolved = resolve_syms m_globals_resolved
    func_imports =
      rtsAsteriusFunctionImports debug <>
      generateFFIFunctionImports bundled_ffi_state

linkStart ::
     Bool
  -> AsteriusStore
  -> S.Set AsteriusEntitySymbol
  -> [AsteriusEntitySymbol]
  -> (Maybe Module, LinkReport)
linkStart debug store root_syms export_funcs =
  ( maybe_result_m
  , report {staticsSymbolMap = ss_sym_map, functionSymbolMap = func_sym_map})
  where
    (maybe_merged_m, report) =
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
    (maybe_result_m, ss_sym_map, func_sym_map) =
      case maybe_merged_m of
        Just merged_m -> (Just result_m, ss_sym_map', func_sym_map')
          where (result_m, ss_sym_map', func_sym_map') =
                  resolveAsteriusModule
                    debug
                    (bundledFFIMarshalState report)
                    export_funcs
                    merged_m
        _ -> (Nothing, mempty, mempty)

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
