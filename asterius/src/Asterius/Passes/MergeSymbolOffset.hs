{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.MergeSymbolOffset
  ( mergeSymbolOffset,
  )
where

import Asterius.EDSL
import Asterius.Internals.SYB
import Asterius.Internals.SafeFromIntegral
import Asterius.Types
import Data.Word
import Type.Reflection

noNeg :: Expression -> Int -> Word32
noNeg e n
  | n >= 0 =
    safeFromIntegral n
  | otherwise =
    error $
      "Asterius.Passes.MergeSymbolOffset.noNeg: negative index "
        <> show n
        <> " when merging "
        <> show e

{-# INLINEABLE mergeSymbolOffset #-}
mergeSymbolOffset :: Monad m => GenericM m
mergeSymbolOffset t =
  pure $ case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl -> case t of
      Load {ptr = Unary {unaryOp = WrapInt64, operand0 = sym@Symbol {..}}, ..} ->
        Load
          { signed = signed,
            bytes = bytes,
            offset = noNeg t $ symbolOffset + safeFromIntegral offset,
            valueType = valueType,
            ptr = wrapInt64 $ sym {symbolOffset = 0}
          }
      Store {ptr = Unary {unaryOp = WrapInt64, operand0 = sym@Symbol {..}}, ..} ->
        Store
          { bytes = bytes,
            offset = noNeg t $ symbolOffset + safeFromIntegral offset,
            ptr = wrapInt64 $ sym {symbolOffset = 0},
            value = value,
            valueType = valueType
          }
      _ -> t
    _ -> t
