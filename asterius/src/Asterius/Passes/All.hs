{-# LANGUAGE RankNTypes #-}

module Asterius.Passes.All
  ( allPasses
  ) where

import Asterius.Internals.SYB
import Asterius.Passes.CCall
import Asterius.Passes.Common
import Asterius.Passes.Events
import Asterius.Passes.GlobalRegs
import Asterius.Passes.LocalRegs
import Asterius.Passes.MergeSymbolOffset
import Asterius.Passes.Relooper
import Asterius.Passes.ResolveSymbols
import Asterius.Types
import Control.Monad.State.Strict
import Data.Data (Data)
import Data.Int
import Data.Map.Strict (Map)
import Data.Set (Set)

{-# INLINABLE allPasses #-}
allPasses ::
     Data a
  => Bool
  -> Map AsteriusEntitySymbol Int64
  -> Set AsteriusEntitySymbol
  -> AsteriusEntitySymbol
  -> FunctionType
  -> Map Event Int
  -> a
  -> (a, [ValueType], Map Event Int)
allPasses debug sym_map export_funcs whoami ft event_map t =
  (result, localRegTable ps, eventMap ps)
  where
    (result, ps) =
      runState (pipeline t) defaultPassesState {eventMap = event_map}
    pipeline =
      everywhereM $
      rewriteEmitEvent <=<
      relooperShallow <=<
      resolveLocalRegs ft <=<
      resolveSymbols sym_map <=<
      mergeSymbolOffset <=<
      maskUnknownCCallTargets whoami export_funcs <=< resolveGlobalRegs
