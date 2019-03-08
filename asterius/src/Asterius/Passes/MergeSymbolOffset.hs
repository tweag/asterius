{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.MergeSymbolOffset
  ( mergeSymbolOffset
  ) where

import Asterius.Internals.SYB
import Asterius.Types
import Type.Reflection

{-# INLINEABLE mergeSymbolOffset #-}
mergeSymbolOffset :: Monad m => GenericM m
mergeSymbolOffset t =
  pure $
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Load {ptr = sym@Symbol {..}, ..} ->
          Load
            { signed = signed
            , bytes = bytes
            , offset = fromIntegral $ symbolOffset + fromIntegral offset
            , valueType = valueType
            , ptr = sym {symbolOffset = 0}
            }
        Store {ptr = sym@Symbol {..}, ..} ->
          Store
            { bytes = bytes
            , offset = fromIntegral $ symbolOffset + fromIntegral offset
            , ptr = sym {symbolOffset = 0}
            , value = value
            , valueType = valueType
            }
        _ -> t
    _ -> t
