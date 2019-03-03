{-# LANGUAGE RankNTypes #-}

module Asterius.Passes.All
  ( allPasses
  ) where

import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Passes.LocalRegs
import Asterius.Passes.Relooper
import Asterius.Types
import Control.Monad.State.Strict
import Data.Data (Data)

allPasses :: Data a => Bool -> FunctionType -> a -> (a, [ValueType])
allPasses debug ft t = (result, localRegTable ps)
  where
    (result, ps) = runState (pipeline t) defaultPassesState
    pipeline = everywhereM $ relooperShallow <=< resolveLocalRegs ft
