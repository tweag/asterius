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
import qualified Data.Vector as V
import Type.Reflection

collectUnresolvedLocalRegs ::
     Data a => a -> HM.HashMap UnresolvedLocalReg ValueType
collectUnresolvedLocalRegs t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        UnresolvedGetLocal {..} -> [(unresolvedLocalReg, valueType)]
        _ -> lrs
    _ -> lrs
  where
    lrs = gmapQr (<>) mempty collectUnresolvedLocalRegs t

resolveLocalRegs :: Data a => a -> (a, V.Vector ValueType)
resolveLocalRegs t = (f t, V.fromList $ I32 : [vt | (_, (vt, _)) <- lrs])
  where
    lrs =
      zipWith
        (\(lr, vt) idx -> (lr, (vt, idx)))
        (HM.toList $ collectUnresolvedLocalRegs t)
        ([1 ..] :: [BinaryenIndex])
    lr_map = HM.fromList lrs
    f :: Data a => a -> a
    f x =
      case eqTypeRep (typeOf x) (typeRep :: TypeRep Expression) of
        Just HRefl ->
          case x of
            UnresolvedGetLocal {..} ->
              GetLocal
                { index = snd $ lr_map HM.! unresolvedLocalReg
                , valueType = valueType
                }
            UnresolvedSetLocal {..} ->
              SetLocal
                {index = snd $ lr_map HM.! unresolvedLocalReg, value = value}
            UnresolvedTeeLocal {..} ->
              TeeLocal
                {index = snd $ lr_map HM.! unresolvedLocalReg, value = value}
            _ -> go
        _ -> go
      where
        go = gmapT f x
