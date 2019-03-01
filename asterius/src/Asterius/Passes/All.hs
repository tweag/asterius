{-# LANGUAGE RankNTypes #-}

module Asterius.Passes.All
  ( allPasses
  ) where

import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Passes.Relooper
import Asterius.Passes.Whoami
import Control.Monad.State.Strict
import Data.Data (Data)

allPasses :: Data a => Bool -> a -> a
allPasses dbg t = result
  where
    init_state = defaultPassesState {debug = dbg}
    result = evalState (pipeline t) init_state
    pipeline = everywhereM $ relooperShallow <=< whoami
