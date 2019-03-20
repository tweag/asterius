{-# LANGUAGE RecordWildCards #-}

module Asterius.Iserv.Link
  ( iservLink
  ) where

import Asterius.Builtins
import Asterius.Iserv.State
import Asterius.Ld
import Asterius.Resolve
import Asterius.Types
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

iservLink :: IservState -> AsteriusEntitySymbol -> (Module, [Event], LinkReport)
iservLink IservState {..} root_sym =
  linkStart
    False
    False
    True
    (rtsAsteriusModule defaultBuiltinsOptions <>
     Map.foldl' (<>) iservArchives iservObjs)
    (Set.singleton root_sym <> rtsUsedSymbols <>
     Set.fromList
       [ AsteriusEntitySymbol {entityName = internalName}
       | FunctionExport {..} <- rtsAsteriusFunctionExports False False
       ])
    []
