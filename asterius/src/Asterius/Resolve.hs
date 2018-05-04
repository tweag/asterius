{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

#include "DerivedConstants.h"

module Asterius.Resolve
  ( resolveLocalRegs
  , unresolvedGlobalRegType
  , collectUnresolvedGlobalRegs
  , resolveGlobalRegs
  , collectAsteriusEntitySymbols
  , mergeSymbols
  , resolveAsteriusModule
  , linkStart
  ) where

import Asterius.Builtins
import Asterius.Internals
import Asterius.Types
import Control.Exception
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapT)
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Foreign
import GHC.Exts
import Type.Reflection ((:~~:)(..), TypeRep, eqTypeRep, typeOf, typeRep)

unresolvedLocalRegType :: UnresolvedLocalReg -> ValueType
unresolvedLocalRegType lr =
  case lr of
    UniqueLocalReg _ vt -> vt
    SwitchCondReg -> I64
    QuotRemI32X -> I32
    QuotRemI32Y -> I32
    QuotRemI64X -> I64
    QuotRemI64Y -> I64

collectUnresolvedLocalRegs :: Data a => a -> HS.HashSet UnresolvedLocalReg
collectUnresolvedLocalRegs = collect proxy#

resolveLocalRegs :: Data a => a -> (a, V.Vector ValueType)
resolveLocalRegs t =
  (f t, V.fromList $ I32 : [unresolvedLocalRegType lr | (lr, _) <- lrs])
  where
    lrs =
      zip (HS.toList $ collectUnresolvedLocalRegs t) ([1 ..] :: [BinaryenIndex])
    lr_map = HM.fromList lrs
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

collectUnresolvedGlobalRegs :: Data a => a -> HS.HashSet UnresolvedGlobalReg
collectUnresolvedGlobalRegs = collect proxy#

resolveGlobalRegs :: Data a => a -> (a, HS.HashSet UnresolvedGlobalReg)
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
          , initValue = UnresolvedOff capabilitySymbol OFFSET_Capability_r
          })
    _ -> throw $ AssignToImmutableGlobalReg gr

collectAsteriusEntitySymbols :: Data a => a -> HS.HashSet AsteriusEntitySymbol
collectAsteriusEntitySymbols = collect proxy#

mergeSymbols ::
     AsteriusStore -> HS.HashSet AsteriusEntitySymbol -> AsteriusModule
mergeSymbols AsteriusStore {..} syms = final_m
  where
    (_, _, final_m) = go (syms, mempty, mempty)
    go i@(_, i_syms, _) =
      if HS.size i_syms == HS.size o_syms
        then o
        else go o
      where
        o@(_, o_syms, _) = iter i
    iter (i_staging_syms, i_syms, i_m) =
      ( collectAsteriusEntitySymbols i_staging_m `HS.difference` o_syms
      , o_syms
      , i_staging_m <> i_m)
      where
        i_staging_m =
          foldMap
            (\staging_sym ->
               case moduleMap ! (symbolMap ! staging_sym) of
                 AsteriusModule {..} ->
                   case HM.lookup staging_sym staticsMap of
                     Just ss -> mempty {staticsMap = [(staging_sym, ss)]}
                     _ ->
                       mempty
                         { functionMap =
                             [(staging_sym, functionMap ! staging_sym)]
                         })
            i_staging_syms
        o_syms = i_staging_syms <> i_syms

makeFunctionTable ::
     AsteriusModule -> (FunctionTable, HM.HashMap AsteriusEntitySymbol Int64)
makeFunctionTable AsteriusModule {..} =
  ( FunctionTable {functionNames = V.fromList $ map entityName func_syms}
  , HM.fromList $ zip func_syms [1 ..])
  where
    func_syms = HM.keys functionMap

roundBy :: Integral a => a -> a -> a
roundBy y x
  | x `mod` y == 0 = x
  | otherwise = ((x `div` y) + 1) * y

makeStaticsOffsetTable ::
     AsteriusModule -> (Int64, HM.HashMap AsteriusEntitySymbol Int64)
makeStaticsOffsetTable AsteriusModule {..} = (last_o, HM.fromList statics_map)
  where
    (last_o, statics_map) = layoutStatics $ HM.toList staticsMap
    layoutStatics = foldl' iterLayoutStaticsState (8, [])
    iterLayoutStaticsState (ptr, sym_map) (ss_sym, ss) =
      ( roundBy 8 $ ptr + fromIntegral (asteriusStaticsSize ss)
      , (ss_sym, ptr) : sym_map)

makeMemory ::
     AsteriusModule -> Int64 -> HM.HashMap AsteriusEntitySymbol Int64 -> Memory
makeMemory AsteriusModule {..} last_o sym_map =
  Memory
    { initialPages = page_num
    , maximumPages = page_num
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
        (sym_map ! ss_sym, [])
        asteriusStatics
    page_num = fromIntegral $ roundBy 65536 last_o `div` 65536

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

resolveAsteriusModule :: AsteriusModule -> Module
resolveAsteriusModule m_unresolved =
  Module
    { functionTypeMap = [(fnTypeName, fnType), (stgRunTypeName, stgRunType)]
    , functionMap' =
        HM.fromList
          [ (entityName func_sym, func)
          | (func_sym, func) <- HM.toList $ functionMap m_resolved
          ]
    , functionImports = []
    , tableImports = []
    , globalImports = []
    , functionExports = []
    , tableExports = []
    , globalExports = []
    , globalMap =
        HM.fromList $
        map (resolve_syms . marshalGlobalReg) $ HS.toList global_regs
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

linkStart :: AsteriusStore -> HS.HashSet AsteriusEntitySymbol -> Module
linkStart store syms = resolveAsteriusModule $ mergeSymbols store syms
