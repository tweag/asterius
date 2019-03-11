{-# LANGUAGE GADTs #-}

module Asterius.Linker.LinkExpr
  ( coreExprModules
  ) where

import qualified CoreSyn as GHC
import Data.Data (Data, gmapQl)
import qualified Module as GHC
import qualified Name as GHC (nameModule_maybe)
import Type.Reflection
import qualified UniqDSet as GHC
import qualified Var as GHC (Var, varName)

externalModules :: Data a => a -> GHC.UniqDSet GHC.Module
externalModules t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep GHC.Var) of
    Just HRefl ->
      case GHC.nameModule_maybe $ GHC.varName t of
        Just m -> GHC.unitUniqDSet m
        _ -> GHC.emptyUniqDSet
    _ -> gmapQl GHC.unionUniqDSets GHC.emptyUniqDSet externalModules t

coreExprModules :: GHC.Expr GHC.Var -> [GHC.Module]
coreExprModules = GHC.uniqDSetToList . externalModules
