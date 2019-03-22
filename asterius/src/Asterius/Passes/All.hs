module Asterius.Passes.All
  ( allPasses
  ) where

import Asterius.Internals.SYB
import Asterius.Passes.Common
import Asterius.Passes.Events
import Asterius.Passes.LocalRegs
import Asterius.Passes.Relooper
import Asterius.Types
import Control.Monad.State.Strict
import Data.Data (Data)

{-# INLINABLE allPasses #-}
allPasses ::
     Data a
  => Bool
  -> Bool
  -> FunctionType
  -> [Event]
  -> a
  -> (a, [ValueType], [Event])
allPasses debug binaryen ft event_stack t =
  (result, localRegTable ps, eventStack ps)
  where
    (result, ps) =
      runState (pipeline t) defaultPassesState {eventStack = event_stack}
    pipeline =
      relooper_pass <=< everywhereM (rewriteEmitEvent <=< resolveLocalRegs ft)
    relooper_pass
      | binaryen = pure
      | otherwise = relooperShallow
