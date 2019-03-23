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

{-# INLINABLE allPasses #-}
allPasses :: Data a => Bool -> Bool -> FunctionType -> a -> (a, [ValueType])
allPasses debug binaryen ft t = (result, localRegTable ps)
  where
    (result, ps) = runState (pipeline t) defaultPassesState
    pipeline = relooper_pass <=< everywhereM (resolveLocalRegs ft)
    relooper_pass
      | binaryen = pure
      | otherwise = relooperShallow
