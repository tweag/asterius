{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.All
  ( adjustLocalRegs
  ) where

import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Passes.LocalRegs
import Asterius.Types
import Control.Monad.State.Strict

{-# INLINEABLE adjustLocalRegs #-}
adjustLocalRegs :: Function -> Function
adjustLocalRegs Function {..} =
  Function
    {functionType = functionType, varTypes = localRegTable ps, body = result}
  where
    (result, ps) = runState (pipeline body) defaultPassesState
    pipeline = everywhereM (resolveLocalRegs functionType)
