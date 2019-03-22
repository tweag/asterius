{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.Events
  ( rewriteEmitEvent
  ) where

import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Types
import Control.Monad.State.Strict
import Type.Reflection

{-# INLINABLE rewriteEmitEvent #-}
rewriteEmitEvent :: MonadState PassesState m => GenericM m
rewriteEmitEvent t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        EmitEvent {..} ->
          state $ \ps@PassesState {..} ->
            ( emit eventCount
            , ps {eventCount = succ eventCount, eventStack = event : eventStack})
        _ -> pure t
    _ -> pure t
  where
    emit i =
      CallImport
        { target' = "__asterius_eventI32"
        , operands = [ConstI32 $ fromIntegral i]
        , callImportReturnTypes = []
        }
