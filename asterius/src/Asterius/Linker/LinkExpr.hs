{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Linker.LinkExpr
  ( linkCoreExpr
  ) where

import Asterius.Iserv.Trace
import qualified CoreSyn as GHC
import Data.Data (Data, gmapQl)
import Data.Foldable
import qualified HscTypes as GHC
import qualified Linker as GHC
import qualified Module as GHC
import qualified Name as GHC (nameModule_maybe)
import qualified Packages as GHC
import Type.Reflection
import qualified UniqDSet as GHC
import qualified Var as GHC (varName)

externalModules :: Data a => a -> GHC.UniqDSet GHC.Module
externalModules t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep GHC.CoreBndr) of
    Just HRefl ->
      case GHC.nameModule_maybe $ GHC.varName t of
        Just m -> GHC.unitUniqDSet m
        _ -> GHC.emptyUniqDSet
    _ -> gmapQl GHC.unionUniqDSets GHC.emptyUniqDSet externalModules t

coreExprModules :: GHC.CoreExpr -> [GHC.Module]
coreExprModules = GHC.uniqDSetToList . externalModules

linkCoreExpr :: Bool -> GHC.HscEnv -> GHC.CoreExpr -> IO ()
linkCoreExpr verbose hsc_env expr = do
  trace verbose "Asterius.Linker.LinkExpr.linkCoreExpr: initDynLinker"
  GHC.initDynLinker hsc_env
  trace verbose "Asterius.Linker.LinkExpr.linkCoreExpr: link ghci"
  let Just ghci_comp_id =
        GHC.lookupPackageName (GHC.hsc_dflags hsc_env) (GHC.PackageName "ghci")
  GHC.linkPackages hsc_env [GHC.componentIdToInstalledUnitId ghci_comp_id]
  trace verbose "Asterius.Linker.LinkExpr.linkCoreExpr: linkDependencies"
  for_ (coreExprModules expr) $ GHC.linkModule hsc_env
