{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.CCall
  ( maskUnknownCCallTargets
  ) where

import Asterius.Builtins
import Asterius.Internals.SYB
import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Type.Reflection

{-# INLINABLE maskUnknownCCallTargets #-}
maskUnknownCCallTargets ::
     Monad m => AsteriusEntitySymbol -> S.Set AsteriusEntitySymbol -> GenericM m
maskUnknownCCallTargets whoami export_funcs t =
  pure $
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Call {..}
          | not $ has_call_target target ->
            emitErrorMessage
              callReturnTypes
              ("Inside " <> entityName whoami <> ", " <> entityName target <>
               " failed: unimplemented stub function entered")
        _ -> t
    _ -> t
  where
    has_call_target sym =
      "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName sym) ||
      sym `M.member` functionMap (rtsAsteriusModule defaultBuiltinsOptions) ||
      sym `S.member` export_funcs
