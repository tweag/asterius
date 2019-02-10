{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Workarounds
  ( maskUnknownCCallTargets
  ) where

import Asterius.Builtins
import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapM)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Type.Reflection

maskUnknownCCallTargets ::
     (Monad m, Data a)
  => AsteriusEntitySymbol
  -> S.Set AsteriusEntitySymbol
  -> a
  -> m a
maskUnknownCCallTargets whoami export_funcs = f
  where
    has_call_target sym =
      "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName sym) ||
      sym `M.member` functionMap (rtsAsteriusModule defaultBuiltinsOptions) ||
      sym `S.member` export_funcs
    f :: (Monad m, Data a) => a -> m a
    f t =
      let go = gmapM f t
       in case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
            Just HRefl ->
              case t of
                Call {..}
                  | not $ has_call_target target ->
                    pure $
                    emitErrorMessage
                      callReturnTypes
                      ("Inside " <> entityName whoami <> ", " <>
                       entityName target <>
                       " failed: unimplemented stub function entered")
                  | target == "createIOThread" ->
                    case operands of
                      [cap, stack_size_w@Load {valueType = I32}, target_closure] ->
                        pure $
                        t
                          { operands =
                              [ cap
                              , Unary
                                  { unaryOp = ExtendUInt32
                                  , operand0 = stack_size_w
                                  }
                              , target_closure
                              ]
                          }
                      _ -> pure t
                  | otherwise -> pure t
                _ -> go
            _ -> go
