module Asterius.Passes.All
  ( allPasses
  ) where

import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Passes.LocalRegs
import Asterius.Passes.Relooper
import Asterius.Types
import Control.Monad.State.Strict

{-# INLINABLE allPasses #-}
allPasses ::
     Bool -> Bool -> FunctionType -> Expression -> (Expression, [ValueType])
allPasses debug binaryen ft t = (relooper_pass result, localRegTable ps)
  where
    (result, ps) = runState (pipeline t) defaultPassesState
    pipeline = everywhereM (resolveLocalRegs ft)
    relooper_pass
      | binaryen = id
      | otherwise = relooperExpression
