{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.Events
  ( rewriteEmitEvent
  , eventTable
  ) where

import Asterius.Internals.Containers
import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Types
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Type.Reflection

rewriteEmitEvent :: MonadState PassesState m => GenericM m
rewriteEmitEvent t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        EmitEvent {..} ->
          state $ \ps@PassesState {..} ->
            case Map.lookup event eventMap of
              Just i -> (emit i, ps)
              _ -> (emit i, ps {eventMap = Map.insert event i eventMap})
                where i = fromIntegral $ Map.size eventMap
        _ -> pure t
    _ -> pure t
  where
    emit i =
      CallImport
        { target' = "__asterius_eventI32"
        , operands = [ConstI32 $ fromIntegral i]
        , callImportReturnTypes = []
        }

eventTable :: PassesState -> [Event]
eventTable PassesState {..} = sortKeysByIntValue eventMap
