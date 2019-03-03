{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

maskUnknownCCallTargets ::
     forall m. Monad m
  => AsteriusEntitySymbol
  -> S.Set AsteriusEntitySymbol
  -> GenericM m
maskUnknownCCallTargets whoami export_funcs = f
  where
    has_call_target sym =
      "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName sym) ||
      sym `M.member` functionMap (rtsAsteriusModule defaultBuiltinsOptions) ||
      sym `S.member` export_funcs
    f :: GenericM m
    f t =
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
              | target == "createIOThread" ->
                case operands of
                  [cap, stack_size_w@Load {valueType = I32}, target_closure] ->
                    t
                      { operands =
                          [ cap
                          , Unary
                              {unaryOp = ExtendUInt32, operand0 = stack_size_w}
                          , target_closure
                          ]
                      }
                  _ -> t
              | otherwise -> t
            _ -> t
        _ -> t
