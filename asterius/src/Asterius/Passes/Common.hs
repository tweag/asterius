{-# LANGUAGE StrictData #-}

module Asterius.Passes.Common
  ( PassesState(..)
  , defaultPassesState
  ) where

import Asterius.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data PassesState = PassesState
  { currentFunction :: AsteriusEntitySymbol
  , eventMap :: Map Event Int
  }

defaultPassesState :: PassesState
defaultPassesState =
  PassesState
    {currentFunction = AsteriusEntitySymbol mempty, eventMap = Map.empty}
