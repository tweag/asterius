{-# LANGUAGE StrictData #-}

module Asterius.Passes.Common
  ( PassesState(..)
  , defaultPassesState
  ) where

import Asterius.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data PassesState = PassesState
  { localRegMap :: Map UnresolvedLocalReg Int
  , eventMap :: Map Event Int
  }

defaultPassesState :: PassesState
defaultPassesState = PassesState {localRegMap = Map.empty, eventMap = Map.empty}
