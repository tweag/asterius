{-# LANGUAGE StrictData #-}

module Asterius.Passes.Common
  ( PassesState(..)
  , defaultPassesState
  ) where

import Asterius.Types

data PassesState = PassesState
  { debug :: Bool
  , currentFunction :: AsteriusEntitySymbol
  }

defaultPassesState :: PassesState
defaultPassesState =
  PassesState {debug = False, currentFunction = AsteriusEntitySymbol mempty}
