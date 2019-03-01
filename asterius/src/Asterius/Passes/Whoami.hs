{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Asterius.Passes.Whoami
  ( whoami
  ) where

import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Types
import Control.Monad.State.Strict
import Type.Reflection

whoami :: MonadState PassesState m => GenericM m
whoami t = do
  modify' $ \s ->
    s
      { currentFunction =
          case eqTypeRep
                 (typeOf t)
                 (typeRep :: TypeRep (AsteriusEntitySymbol, AsteriusFunction)) of
            Just HRefl -> fst t
            _ ->
              case eqTypeRep
                     (typeOf t)
                     (typeRep :: TypeRep (AsteriusEntitySymbol, Function)) of
                Just HRefl -> fst t
                _ -> currentFunction s
      }
  pure t
