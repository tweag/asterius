{-# LANGUAGE RecordWildCards #-}

module Asterius.Iserv.Link
  ( iservLink
  ) where

import Asterius.Builtins
import Asterius.Iserv.State
import Asterius.Ld
import Asterius.Resolve
import Asterius.Types
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

iservLink ::
     IservState -> AsteriusEntitySymbol -> IO (Module, [Event], LinkReport)
iservLink IservState {..} root_sym = do
  IservModules {..} <- readIORef iservModulesRef
  pure $
    linkStart
      False
      False
      True
      False
      (rtsAsteriusModule defaultBuiltinsOptions <>
       Map.foldl' (<>) iservArchives iservObjs)
      (Set.singleton root_sym <> rtsUsedSymbols <>
       Set.fromList
         [ AsteriusEntitySymbol {entityName = internalName}
         | FunctionExport {..} <- rtsFunctionExports False False
         ])
      []
