{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Resolve
  ( resolveLocalRegs
  ) where

import Asterius.Types
import Data.Data (Data, gmapQr, gmapT)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Type.Reflection

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
collectUnresolvedLocalRegs t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep UnresolvedLocalReg) of
    Just HRefl -> [t]
    _ -> gmapQr (<>) mempty collectUnresolvedLocalRegs t

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
                {index = lr_idx unresolvedLocalReg, valueType = undefined}
            UnresolvedSetLocal {..} ->
              SetLocal {index = lr_idx unresolvedLocalReg, value = f value}
            UnresolvedTeeLocal {..} ->
              TeeLocal {index = lr_idx unresolvedLocalReg, value = f value}
            _ -> go
        _ -> go
      where
        go = gmapT f x
