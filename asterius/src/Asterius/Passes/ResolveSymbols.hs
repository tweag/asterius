{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.ResolveSymbols
  ( resolveSymbols
  ) where

import Asterius.Builtins
import Asterius.Internals.SYB
import Asterius.Types
import Data.Coerce
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Type.Reflection

{-# INLINABLE resolveSymbols #-}
resolveSymbols :: Monad m => Map AsteriusEntitySymbol Int64 -> GenericM m
resolveSymbols sym_map t =
  pure $
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Symbol {..} ->
          case Map.lookup unresolvedSymbol sym_map of
            Just addr -> t {resolvedSymbol = Just addr}
            _ ->
              emitErrorMessage [I64] $
              "Unresolved symbol: " <> coerce unresolvedSymbol
        _ -> t
    _ -> t
