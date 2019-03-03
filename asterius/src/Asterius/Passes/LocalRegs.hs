{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Passes.LocalRegs
  ( resolveLocalRegs
  , localRegTable
  ) where

import Asterius.Internals.Containers
import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Types
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Type.Reflection

unresolvedLocalRegValueType :: UnresolvedLocalReg -> ValueType
unresolvedLocalRegValueType reg =
  case reg of
    UniqueLocalReg _ vt -> vt
    QuotRemI32X -> I32
    QuotRemI32Y -> I32
    QuotRemI64X -> I64
    QuotRemI64Y -> I64

resolveLocalRegs ::
     forall m. MonadState PassesState m
  => FunctionType
  -> GenericM m
resolveLocalRegs FunctionType {..} t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        UnresolvedGetLocal {..} ->
          (\i ->
             GetLocal
               { index = i
               , valueType = unresolvedLocalRegValueType unresolvedLocalReg
               }) <$>
          idx unresolvedLocalReg
        UnresolvedSetLocal {..} ->
          (\i -> SetLocal {index = i, value = value}) <$> idx unresolvedLocalReg
        _ -> pure t
    _ -> pure t
  where
    base_idx = 3 + length paramTypes
    idx :: UnresolvedLocalReg -> m BinaryenIndex
    idx reg =
      state $ \ps@PassesState {..} ->
        case Map.lookup reg localRegMap of
          Just i -> (fromIntegral i, ps)
          _ -> (fromIntegral i, ps {localRegMap = Map.insert reg i localRegMap})
            where i = base_idx + Map.size localRegMap

localRegTable :: PassesState -> [ValueType]
localRegTable PassesState {..} =
  I32 :
  I32 : I64 : map unresolvedLocalRegValueType (sortKeysByIntValue localRegMap)
