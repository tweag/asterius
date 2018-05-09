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
  , collectUnresolvedGlobalRegs
  , resolveGlobalRegs
  , collectAsteriusEntitySymbols
  , LinkReport(..)
  , mergeSymbols
  , resolveAsteriusModule
  , linkStart
  ) where

import Asterius.Builtins
import Asterius.Containers
import Asterius.Internals
import Asterius.Types
import Control.Exception
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapT)
import Data.Either
import Data.Foldable
import Data.String
import qualified Data.Vector as V
import Foreign
import GHC.Exts (fromList, proxy#)
import Language.Haskell.GHC.Toolkit.Constants
import Type.Reflection ((:~~:)(..), TypeRep, eqTypeRep, typeOf, typeRep)

unresolvedLocalRegType :: UnresolvedLocalReg -> ValueType
unresolvedLocalRegType lr =
  case lr of
    UniqueLocalReg _ vt -> vt
    QuotRemI32X -> I32
    QuotRemI32Y -> I32
    QuotRemI64X -> I64
    QuotRemI64Y -> I64

collectUnresolvedLocalRegs :: Data a => a -> HashSet UnresolvedLocalReg
collectUnresolvedLocalRegs = collect proxy#

resolveLocalRegs :: Data a => a -> (a, V.Vector ValueType)
resolveLocalRegs t =
  (f t, V.fromList $ I32 : I64 : [unresolvedLocalRegType lr | (lr, _) <- lrs])
  where
    lrs =
      zip (toList $ collectUnresolvedLocalRegs t) ([2 ..] :: [BinaryenIndex])
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

collectUnresolvedGlobalRegs :: Data a => a -> HashSet UnresolvedGlobalReg
collectUnresolvedGlobalRegs = collect proxy#

resolveGlobalRegs :: Data a => a -> (a, HashSet UnresolvedGlobalReg)
resolveGlobalRegs t = (f t, collectUnresolvedGlobalRegs t)
  where
    f :: Data a => a -> a
    f x =
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
                {name = globalRegName unresolvedGlobalReg, value = f value}
            _ -> go
        _ -> go
      where
        go = gmapT f x

globalRegName :: UnresolvedGlobalReg -> SBS.ShortByteString
globalRegName = fst . marshalGlobalReg

marshalGlobalReg :: UnresolvedGlobalReg -> (SBS.ShortByteString, Global)
marshalGlobalReg gr =
  case gr of
    VanillaReg i ->
      ( fromString $ "_asterius_R" <> show i
      , Global {valueType = I64, mutable = True, initValue = ConstI64 0})
    FloatReg i ->
      ( fromString $ "_asterius_F" <> show i
      , Global {valueType = F32, mutable = True, initValue = ConstF32 0})
    DoubleReg i ->
      ( fromString $ "_asterius_D" <> show i
      , Global {valueType = F64, mutable = True, initValue = ConstF64 0})
    LongReg i ->
      ( fromString $ "_asterius_L" <> show i
      , Global {valueType = I64, mutable = True, initValue = ConstI64 0})
    Sp ->
      ( "_asterius_Sp"
      , Global {valueType = I64, mutable = True, initValue = ConstI64 0})
    SpLim ->
      ( "_asterius_SpLim"
      , Global {valueType = I64, mutable = True, initValue = ConstI64 0})
    Hp ->
      ( "_asterius_Hp"
      , Global {valueType = I64, mutable = True, initValue = ConstI64 0})
    HpLim ->
      ( "_asterius_HpLim"
      , Global {valueType = I64, mutable = True, initValue = ConstI64 0})
    CurrentNursery ->
      ( "_asterius_CurrentNursery"
      , Global
          {valueType = I64, mutable = True, initValue = Unresolved bdescrSymbol})
    HpAlloc ->
      ( "_asterius_HpAlloc"
      , Global {valueType = I64, mutable = True, initValue = ConstI64 0})
    BaseReg ->
      ( "_asterius_BaseReg"
      , Global
          { valueType = I64
          , mutable = True
          , initValue = UnresolvedOff capabilitySymbol offset_Capability_r
          })
    _ -> throw $ AssignToImmutableGlobalReg gr

collectAsteriusEntitySymbols :: Data a => a -> HashSet AsteriusEntitySymbol
collectAsteriusEntitySymbols = collect proxy#

data LinkReport = LinkReport
  { parentSymbols :: HashMap AsteriusEntitySymbol (HashSet AsteriusEntitySymbol)
  , unfoundSymbols, unavailableSymbols :: HashSet AsteriusEntitySymbol
  } deriving (Show)

instance Semigroup LinkReport where
  r0 <> r1 =
    LinkReport
      { parentSymbols =
          hashMapUnionWith (<>) (parentSymbols r0) (parentSymbols r1)
      , unfoundSymbols = unfoundSymbols r0 <> unfoundSymbols r1
      , unavailableSymbols = unavailableSymbols r0 <> unavailableSymbols r1
      }

instance Monoid LinkReport where
  mempty =
    LinkReport
      { parentSymbols = mempty
      , unfoundSymbols = mempty
      , unavailableSymbols = mempty
      }

mergeSymbols ::
     AsteriusStore
  -> HashSet AsteriusEntitySymbol
  -> (Maybe AsteriusModule, LinkReport)
mergeSymbols AsteriusStore {..} syms = (maybe_final_m, final_rep)
  where
    maybe_final_m
      | hashSetNull (unfoundSymbols final_rep) &&
          hashSetNull (unavailableSymbols final_rep) = Just final_m
      | otherwise = Nothing
    (_, final_rep, final_m) = go (syms, mempty, mempty)
    go i@(_, i_rep, _)
      | hashMapSize (parentSymbols i_rep) == hashMapSize (parentSymbols o_rep) =
        o
      | otherwise = go o
      where
        o@(_, o_rep, _) = iter i
    iter (i_staging_syms, i_rep, i_m) = (o_staging_syms, o_rep, o_m)
      where
        (i_unfound_syms, i_sym_mods) =
          partitionEithers
            [ case hashMapLookup i_staging_sym symbolMap of
              Just mod_sym -> Right (i_staging_sym, moduleMap ! mod_sym)
              _ -> Left i_staging_sym
            | i_staging_sym <- toList i_staging_syms
            ]
        (i_unavailable_syms, i_sym_modlets) =
          partitionEithers
            [ case hashMapLookup i_staging_sym staticsMap of
              Just ss ->
                Right
                  (i_staging_sym, mempty {staticsMap = [(i_staging_sym, ss)]})
              _ ->
                case hashMapLookup i_staging_sym functionMap of
                  Just func ->
                    Right
                      ( i_staging_sym
                      , mempty {functionMap = [(i_staging_sym, func)]})
                  _ -> Left i_staging_sym
            | (i_staging_sym, AsteriusModule {..}) <- i_sym_mods
            ]
        i_mod = mconcat $ map snd i_sym_modlets
        i_parent_map =
          hashMapFromListWith
            (<>)
            [ (i_descendent_sym, [i_staging_sym])
            | (i_staging_sym, i_modlet) <- i_sym_modlets
            , i_descendent_sym <- toList $ collectAsteriusEntitySymbols i_modlet
            ]
        o_rep =
          mempty
            { parentSymbols = i_parent_map
            , unfoundSymbols = fromList i_unfound_syms
            , unavailableSymbols = fromList i_unavailable_syms
            } <>
          i_rep
        o_m = i_mod <> i_m
        o_staging_syms =
          fromList (hashMapKeys i_parent_map) `hashSetDifference`
          hashSetUnions
            [ unfoundSymbols o_rep
            , unavailableSymbols o_rep
            , hashSetUnions $ hashMapElems $ parentSymbols o_rep
            ]

makeFunctionTable ::
     AsteriusModule -> (FunctionTable, HashMap AsteriusEntitySymbol Int64)
makeFunctionTable AsteriusModule {..} =
  ( FunctionTable {functionNames = V.fromList $ map entityName func_syms}
  , fromList $ zip func_syms [1 ..])
  where
    func_syms = hashMapKeys functionMap

roundBy :: Integral a => a -> a -> a
roundBy y x
  | x `mod` y == 0 = x
  | otherwise = ((x `div` y) + 1) * y

makeStaticsOffsetTable ::
     AsteriusModule -> (Int64, HashMap AsteriusEntitySymbol Int64)
makeStaticsOffsetTable AsteriusModule {..} = (last_o, fromList statics_map)
  where
    (last_o, statics_map) = layoutStatics $ hashMapToList staticsMap
    layoutStatics = foldl' iterLayoutStaticsState (8, [])
    iterLayoutStaticsState (ptr, sym_map) (ss_sym, ss) =
      ( roundBy 8 $ ptr + fromIntegral (asteriusStaticsSize ss)
      , (ss_sym, ptr) : sym_map)

makeMemory ::
     AsteriusModule -> Int64 -> HashMap AsteriusEntitySymbol Int64 -> Memory
makeMemory AsteriusModule {..} last_o sym_map =
  Memory
    { initialPages = page_num
    , maximumPages = page_num
    , exportName = ""
    , dataSegments = V.fromList $ concatMap data_segs $ hashMapToList staticsMap
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
    page_num = fromIntegral $ roundBy 65536 last_o `div` 65536

resolveEntitySymbols :: Data a => HashMap AsteriusEntitySymbol Int64 -> a -> a
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

resolveAsteriusModule :: AsteriusModule -> Module
resolveAsteriusModule m_unresolved =
  Module
    { functionTypeMap = rtsAsteriusFunctionTypeMap
    , functionMap' =
        fromList
          [ (entityName func_sym, func)
          | (func_sym, func) <- hashMapToList $ functionMap m_resolved
          ]
    , functionImports = []
    , tableImports = []
    , globalImports = []
    , functionExports = []
    , tableExports = []
    , globalExports = []
    , globalMap =
        fromList (map (resolve_syms . marshalGlobalReg) $ toList global_regs) <>
        rtsAsteriusGlobalMap
    , functionTable = Just func_table
    , memory = Just $ makeMemory m_resolved last_o ss_sym_map
    , startFunctionName = Nothing
    }
  where
    (func_table, func_sym_map) = makeFunctionTable m_unresolved
    (last_o, ss_sym_map) = makeStaticsOffsetTable m_unresolved
    resolve_syms :: Data a => a -> a
    resolve_syms = resolveEntitySymbols $ func_sym_map <> ss_sym_map
    (m_resolved, global_regs) = resolveGlobalRegs $ resolve_syms m_unresolved

linkStart ::
     AsteriusStore -> HashSet AsteriusEntitySymbol -> (Maybe Module, LinkReport)
linkStart store syms = (resolveAsteriusModule <$> maybe_merged_m, report)
  where
    (maybe_merged_m, report) = mergeSymbols store syms
