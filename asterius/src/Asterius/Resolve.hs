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
  , linkStart
  ) where

import Asterius.Builtins
import Asterius.Internals
import Asterius.SymbolDB
import Asterius.Types
import Control.Exception
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapT)
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
      , Global {valueType = I64, mutable = True, initValue = undefined})
    SpLim ->
      ( "_asterius_SpLim"
      , Global {valueType = I64, mutable = True, initValue = undefined})
    Hp ->
      ( "_asterius_Hp"
      , Global {valueType = I64, mutable = True, initValue = undefined})
    HpLim ->
      ( "_asterius_HpLim"
      , Global {valueType = I64, mutable = True, initValue = undefined})
    CurrentNursery ->
      ( "_asterius_CurrentNursery"
      , Global {valueType = I64, mutable = True, initValue = undefined})
    HpAlloc ->
      ( "_asterius_HpAlloc"
      , Global {valueType = I64, mutable = True, initValue = undefined})
    BaseReg ->
      ( "_asterius_BaseReg"
      , Global
          { valueType = I64
          , mutable = True
          , initValue =
              Binary
                { binaryOp = AddInt64
                , operand0 = undefined
                , operand1 = ConstI64 OFFSET_Capability_r
                }
          })
    _ -> throw $ AssignToImmutableGlobalReg gr

mergeSymbols ::
     AsteriusStore -> HS.HashSet AsteriusEntitySymbol -> AsteriusModule
mergeSymbols store =
  foldMap
    (\sym ->
       availStatus $
       queryAsteriusEntitySymbol
         store
         sym
         (\ss -> mempty {staticsMap = [(sym, ss)]})
         (\f -> mempty {functionMap = [(sym, f)]}))

makeFunctionTable ::
     AsteriusModule -> (FunctionTable, HM.HashMap AsteriusEntitySymbol Int64)
makeFunctionTable = undefined

makeMemory :: AsteriusModule -> (Memory, HM.HashMap AsteriusEntitySymbol Int64)
makeMemory = undefined

resolveEntitySymbols ::
     Data a => HM.HashMap AsteriusEntitySymbol Int64 -> a -> a
resolveEntitySymbols = undefined

resolveAsteriusModule :: AsteriusModule -> Module
resolveAsteriusModule m_unresolved =
  Module
    { functionTypeMap = [(fnTypeName, fnType)]
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
    , globalMap = HM.fromList $ map marshalGlobalReg $ HS.toList global_regs
    , functionTable = Just func_table
    , memory = Just mem
    , startFunctionName = Nothing
    }
  where
    (func_table, func_sym_map) = makeFunctionTable m_unresolved
    (mem, ss_sym_map) = makeMemory m_unresolved
    (m_resolved, global_regs) =
      resolveGlobalRegs
        (resolveEntitySymbols (func_sym_map <> ss_sym_map) m_unresolved)

linkStart :: AsteriusStore -> HS.HashSet AsteriusEntitySymbol -> Module
linkStart store syms = resolveAsteriusModule $ mergeSymbols store syms
