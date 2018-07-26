{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
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
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List
import qualified Data.Vector as V
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
  V.foldl' (\tot s -> tot + asteriusStaticSize s) 0 (asteriusStatics ss)

unresolvedLocalRegType :: UnresolvedLocalReg -> ValueType
unresolvedLocalRegType lr =
  case lr of
    UniqueLocalReg _ vt -> vt
    QuotRemI32X -> I32
    QuotRemI32Y -> I32
    QuotRemI64X -> I64
    QuotRemI64Y -> I64

collectUnresolvedLocalRegs :: Data a => a -> HS.HashSet UnresolvedLocalReg
collectUnresolvedLocalRegs = collect proxy#

resolveLocalRegs :: Data a => Int -> a -> (a, V.Vector ValueType)
resolveLocalRegs func_param_n t =
  ( f t
  , V.fromList $ I32 : I32 : I64 : [unresolvedLocalRegType lr | (lr, _) <- lrs])
  where
    lrs =
      zip
        (sort $ toList $ collectUnresolvedLocalRegs t)
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

collectAsteriusEntitySymbols :: Data a => a -> HS.HashSet AsteriusEntitySymbol
collectAsteriusEntitySymbols = collect proxy#

data LinkReport = LinkReport
  { childSymbols :: HM.HashMap AsteriusEntitySymbol (HS.HashSet AsteriusEntitySymbol)
  , unfoundSymbols, unavailableSymbols :: HS.HashSet AsteriusEntitySymbol
  , staticsSymbolMap, functionSymbolMap :: HM.HashMap AsteriusEntitySymbol Int64
  } deriving (Show)

instance Semigroup LinkReport where
  r0 <> r1 =
    LinkReport
      { childSymbols = HM.unionWith (<>) (childSymbols r0) (childSymbols r1)
      , unfoundSymbols = unfoundSymbols r0 <> unfoundSymbols r1
      , unavailableSymbols = unavailableSymbols r0 <> unavailableSymbols r1
      , staticsSymbolMap = staticsSymbolMap r0 <> staticsSymbolMap r1
      , functionSymbolMap = functionSymbolMap r0 <> functionSymbolMap r1
      }

instance Monoid LinkReport where
  mempty =
    LinkReport
      { childSymbols = mempty
      , unfoundSymbols = mempty
      , unavailableSymbols = mempty
      , staticsSymbolMap = mempty
      , functionSymbolMap = mempty
      }

mergeSymbols ::
     Bool
  -> AsteriusStore
  -> HS.HashSet AsteriusEntitySymbol
  -> (Maybe AsteriusModule, LinkReport)
mergeSymbols debug AsteriusStore {..} syms = (maybe_final_m, final_rep)
  where
    maybe_final_m
      | HS.null (unfoundSymbols final_rep) &&
          HS.null (unavailableSymbols final_rep) = Just final_m
      | otherwise = Nothing
    (_, final_rep, final_m) = go (syms, mempty, mempty)
    go i@(i_staging_syms, _, _)
      | HS.null i_staging_syms = o
      | otherwise = go o
      where
        o = iter i
    iter (i_staging_syms, i_rep, i_m) = (o_staging_syms, o_rep, o_m)
      where
        (i_unfound_syms, i_sym_mods) =
          partitionEithers
            [ case HM.lookup i_staging_sym symbolMap of
              Just mod_sym -> Right (i_staging_sym, moduleMap ! mod_sym)
              _ -> Left i_staging_sym
            | i_staging_sym <- toList i_staging_syms
            ]
        (i_unavailable_syms, i_sym_modlets) =
          partitionEithers
            [ case HM.lookup i_staging_sym staticsMap of
              Just ss ->
                Right
                  ( i_staging_sym
                  , mempty
                      {staticsMap = [(i_staging_sym, resolveGlobalRegs ss)]})
              _ ->
                case HM.lookup i_staging_sym functionMap of
                  Just func ->
                    Right
                      ( i_staging_sym
                      , let m0 =
                              mempty
                                { functionMap =
                                    [ ( i_staging_sym
                                      , patchWritePtrArrayOp $
                                        maskUnknownCCallTargets $
                                        resolveGlobalRegs func)
                                    ]
                                }
                         in if debug
                              then addMemoryTrap m0
                              else m0)
                  _
                    | HM.member i_staging_sym functionErrorMap ->
                      Right
                        ( i_staging_sym
                        , mempty
                            { functionMap =
                                [ ( i_staging_sym
                                  , AsteriusFunction
                                      { functionType =
                                          FunctionType
                                            {returnType = I64, paramTypes = []}
                                      , body =
                                          marshalErrorCode errBrokenFunction I64
                                      })
                                ]
                            })
                    | otherwise -> Left i_staging_sym
            | (i_staging_sym, AsteriusModule {..}) <- i_sym_mods
            ]
        i_child_map =
          fromList
            [ ( i_staging_sym
              , HS.filter (/= i_staging_sym) $
                collectAsteriusEntitySymbols i_modlet)
            | (i_staging_sym, i_modlet) <- i_sym_modlets
            ]
        o_rep =
          mempty
            { childSymbols = i_child_map
            , unfoundSymbols = fromList i_unfound_syms
            , unavailableSymbols = fromList i_unavailable_syms
            } <>
          i_rep
        o_m = mconcat (map snd i_sym_modlets) <> i_m
        o_staging_syms =
          mconcat (HM.elems $ childSymbols o_rep) `HS.difference`
          HS.unions
            [ unfoundSymbols o_rep
            , unavailableSymbols o_rep
            , fromList $ HM.keys $ childSymbols o_rep
            ]

makeFunctionTable ::
     AsteriusModule -> (FunctionTable, HM.HashMap AsteriusEntitySymbol Int64)
makeFunctionTable AsteriusModule {..} =
  ( FunctionTable {functionNames = V.fromList $ map entityName func_syms}
  , fromList $ zip func_syms [1 ..])
  where
    func_syms = sort $ HM.keys functionMap

makeStaticsOffsetTable ::
     AsteriusModule -> (Int64, HM.HashMap AsteriusEntitySymbol Int64)
makeStaticsOffsetTable AsteriusModule {..} = (last_o, fromList statics_map)
  where
    (last_o, statics_map) = layoutStatics $ sortOn fst $ HM.toList staticsMap
    layoutStatics = foldl' iterLayoutStaticsState (8, [])
    iterLayoutStaticsState (ptr, sym_map) (ss_sym, ss) =
      ( fromIntegral $
        8 * roundup_bytes_to_words (fromIntegral ptr + asteriusStaticsSize ss)
      , (ss_sym, ptr) : sym_map)

makeMemory ::
     AsteriusModule -> Int64 -> HM.HashMap AsteriusEntitySymbol Int64 -> Memory
makeMemory AsteriusModule {..} last_o sym_map =
  Memory
    { initialPages =
        fromIntegral $
        roundup (fromIntegral last_o) mblock_size `div` wasmPageSize
    , maximumPages = 65535
    , exportName = "mem"
    , dataSegments =
        V.fromList $ combine $ concatMap data_segs $ HM.toList staticsMap
    }
  where
    data_segs (ss_sym, AsteriusStatics {..}) =
      snd $
      V.foldl'
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

resolveEntitySymbols ::
     Data a => HM.HashMap AsteriusEntitySymbol Int64 -> a -> a
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
                  Serialized (encodePrim (subst unresolvedSymbol))
                UnresolvedOffStatic unresolvedSymbol offset' ->
                  Serialized
                    (encodePrim (subst unresolvedSymbol + fromIntegral offset'))
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
  -> AsteriusModule
  -> ( Module
     , HM.HashMap AsteriusEntitySymbol Int64
     , HM.HashMap AsteriusEntitySymbol Int64)
resolveAsteriusModule debug ffi_state m_globals_resolved =
  ( Module
      { functionTypeMap =
          HM.fromList
            [ (generateWasmFunctionTypeName ft, ft)
            | ft <-
                [functionType | AsteriusFunctionImport {..} <- func_imports] <>
                [ functionType
                | AsteriusFunction {..} <-
                    HM.elems $ functionMap m_globals_syms_resolved
                ]
            ]
      , functionMap' =
          HM.fromList
            [ ( entityName func_sym
              , relooperDeep $
                if debug
                  then addTracingModule func_sym_map func_sym functionType func
                  else func)
            | (func_sym, AsteriusFunction {..}) <-
                HM.toList $ functionMap m_globals_syms_resolved
            , let (body_locals_resolved, local_types) =
                    resolveLocalRegs (V.length $ paramTypes functionType) body
                  func =
                    Function
                      { functionTypeName =
                          generateWasmFunctionTypeName functionType
                      , varTypes = local_types
                      , body = body_locals_resolved
                      }
            ]
      , functionImports =
          V.fromList [resolveFunctionImport imp | imp <- func_imports]
      , tableImports = []
      , globalImports = []
      , functionExports = rtsAsteriusFunctionExports debug
      , tableExports = []
      , globalExports = []
      , globalMap = []
      , functionTable = Just func_table
      , memory = Just $ makeMemory m_globals_syms_resolved last_o ss_sym_map
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
      rtsAsteriusFunctionImports debug <> generateFFIFunctionImports ffi_state

linkStart ::
     Bool
  -> FFIMarshalState
  -> AsteriusStore
  -> HS.HashSet AsteriusEntitySymbol
  -> (Maybe Module, LinkReport)
linkStart debug ffi_state store syms =
  ( maybe_result_m
  , report {staticsSymbolMap = ss_sym_map, functionSymbolMap = func_sym_map})
  where
    bundled_store = generateFFIWrapperStore ffi_state <> store
    (maybe_merged_m, report) = mergeSymbols debug bundled_store syms
    (maybe_result_m, ss_sym_map, func_sym_map) =
      case maybe_merged_m of
        Just merged_m -> (Just result_m, ss_sym_map', func_sym_map')
          where (result_m, ss_sym_map', func_sym_map') =
                  resolveAsteriusModule debug ffi_state merged_m
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
    | (u, vs) <- HM.toList childSymbols
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
