{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Resolve
  ( resolveLocalRegs
  , unresolvedGlobalRegType
  , resolveGlobalRegs
  , LinkReport(..)
  , linkStart
  , renderDot
  , writeDot
  ) where

import Asterius.Builtins
import Asterius.Internals
import Asterius.Tracing
import Asterius.Types
import Control.Exception
import Data.ByteString.Builder
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapT)
import Data.Either
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.String
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

resolveLocalRegs :: Data a => a -> (a, V.Vector ValueType)
resolveLocalRegs t =
  (f t, V.fromList $ I32 : I64 : [unresolvedLocalRegType lr | (lr, _) <- lrs])
  where
    lrs =
      zip (toList $ collectUnresolvedLocalRegs t) ([2 ..] :: [BinaryenIndex])
    lr_map = fromList lrs
    lr_idx = (lr_map HM.!)
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
            UnresolvedTeeLocal {..} ->
              TeeLocal {index = lr_idx unresolvedLocalReg, value = f value}
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

resolveGlobalRegs :: Data a => a -> a
resolveGlobalRegs x =
  case eqTypeRep (typeOf x) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case x of
        UnresolvedGetGlobal {..} ->
          GetGlobal
            { name = globalRegName unresolvedGlobalReg
            , valueType = unresolvedGlobalRegType unresolvedGlobalReg
            }
        UnresolvedSetGlobal {..} ->
          SetGlobal
            { name = globalRegName unresolvedGlobalReg
            , value = resolveGlobalRegs value
            }
        _ -> go
    _ -> go
  where
    go = gmapT resolveGlobalRegs x

globalRegName :: UnresolvedGlobalReg -> SBS.ShortByteString
globalRegName gr =
  case gr of
    VanillaReg i
      | i >= 1 && i <= 10 -> rn "R" i
    FloatReg i
      | i >= 1 && i <= 6 -> rn "F" i
    DoubleReg i
      | i >= 1 && i <= 6 -> rn "D" i
    LongReg 1 -> "L1"
    Sp -> "Sp"
    SpLim -> "SpLim"
    Hp -> "Hp"
    HpLim -> "HpLim"
    CCCS -> "CCCS"
    CurrentTSO -> "CurrentTSO"
    CurrentNursery -> "CurrentNursery"
    HpAlloc -> "HpAlloc"
    BaseReg -> "BaseReg"
    _ -> throw $ AssignToImmutableGlobalReg gr
  where
    rn :: String -> Int -> SBS.ShortByteString
    rn p i = fromString $ p <> show i

collectAsteriusEntitySymbols :: Data a => a -> HS.HashSet AsteriusEntitySymbol
collectAsteriusEntitySymbols = collect proxy#

data LinkReport = LinkReport
  { childSymbols :: HM.HashMap AsteriusEntitySymbol (HS.HashSet AsteriusEntitySymbol)
  , unfoundSymbols, unavailableSymbols :: HS.HashSet AsteriusEntitySymbol
  , functionSymbolMap :: HM.HashMap AsteriusEntitySymbol Int64
  } deriving (Show)

instance Semigroup LinkReport where
  r0 <> r1 =
    LinkReport
      { childSymbols = HM.unionWith (<>) (childSymbols r0) (childSymbols r1)
      , unfoundSymbols = unfoundSymbols r0 <> unfoundSymbols r1
      , unavailableSymbols = unavailableSymbols r0 <> unavailableSymbols r1
      , functionSymbolMap = functionSymbolMap r0 <> functionSymbolMap r1
      }

instance Monoid LinkReport where
  mempty =
    LinkReport
      { childSymbols = mempty
      , unfoundSymbols = mempty
      , unavailableSymbols = mempty
      , functionSymbolMap = mempty
      }

mergeSymbols ::
     AsteriusStore
  -> HS.HashSet AsteriusEntitySymbol
  -> (Maybe AsteriusModule, LinkReport)
mergeSymbols AsteriusStore {..} syms = (maybe_final_m, final_rep)
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
              Just mod_sym -> Right (i_staging_sym, moduleMap HM.! mod_sym)
              _ -> Left i_staging_sym
            | i_staging_sym <- toList i_staging_syms
            ]
        (i_unavailable_syms, i_sym_modlets) =
          partitionEithers
            [ case HM.lookup i_staging_sym staticsMap of
              Just ss ->
                Right
                  (i_staging_sym, mempty {staticsMap = [(i_staging_sym, ss)]})
              _ ->
                case HM.lookup i_staging_sym functionMap of
                  Just func ->
                    Right
                      ( i_staging_sym
                      , mempty {functionMap = [(i_staging_sym, func)]})
                  _ -> Left i_staging_sym
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
    func_syms = HM.keys functionMap

makeStaticsOffsetTable ::
     AsteriusModule -> (Int64, HM.HashMap AsteriusEntitySymbol Int64)
makeStaticsOffsetTable AsteriusModule {..} = (last_o, fromList statics_map)
  where
    (last_o, statics_map) = layoutStatics $ HM.toList staticsMap
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
    , exportName = ""
    , dataSegments = V.fromList $ concatMap data_segs $ HM.toList staticsMap
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
        (sym_map HM.! ss_sym, [])
        asteriusStatics

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
        subst = (sym_table HM.!)

resolveAsteriusModule ::
     Bool -> AsteriusModule -> (Module, HM.HashMap AsteriusEntitySymbol Int64)
resolveAsteriusModule add_tracing m_unresolved =
  ( Module
      { functionTypeMap = rtsAsteriusFunctionTypeMap
      , functionMap' =
          fromList
            [ ( entityName func_sym
              , if add_tracing
                  then addTracingModule func_sym_map func_sym func
                  else func)
            | (func_sym, func) <- HM.toList $ functionMap m_resolved
            ]
      , functionImports = rtsAsteriusFunctionImports
      , tableImports = []
      , globalImports = []
      , functionExports = rtsAsteriusFunctionExports
      , tableExports = []
      , globalExports = []
      , globalMap = resolve_syms rtsAsteriusGlobalMap
      , functionTable = Just func_table
      , memory = Just $ makeMemory m_resolved last_o ss_sym_map
      , startFunctionName = Nothing
      }
  , func_sym_map)
  where
    (func_table, func_sym_map) = makeFunctionTable m_unresolved
    (last_o, ss_sym_map) = makeStaticsOffsetTable m_unresolved
    resolve_syms :: Data a => a -> a
    resolve_syms = resolveEntitySymbols $ func_sym_map <> ss_sym_map
    m_resolved = resolve_syms m_unresolved

linkStart ::
     Bool
  -> AsteriusStore
  -> HS.HashSet AsteriusEntitySymbol
  -> (Maybe Module, LinkReport)
linkStart add_tracing store syms =
  (maybe_result_m, report {functionSymbolMap = func_sym_map})
  where
    (maybe_merged_m, report) = mergeSymbols store syms
    (maybe_result_m, func_sym_map) =
      case maybe_merged_m of
        Just merged_m -> (Just result_m, func_sym_map')
          where (result_m, func_sym_map') =
                  resolveAsteriusModule add_tracing merged_m
        _ -> (Nothing, mempty)

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
