{-# LANGUAGE RankNTypes #-}

module Asterius.Passes.All
  ( allPasses
  ) where

import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Passes.Relooper
import Control.Monad.State.Strict
import Data.Data (Data)

allPasses :: Data a => Bool -> a -> a
allPasses debug t = result
  where
    init_state = defaultPassesState
    result = evalState (pipeline t) init_state
    pipeline = everywhereM relooperShallow
