{-# LANGUAGE GADTs #-}

module Asterius.Linker.LinkExpr
  ( linkCoreExpr
  ) where

import Asterius.Iserv.Trace
import qualified BasicTypes as GHC
import Control.Monad
import qualified CoreSyn as GHC
import Data.Data (Data, gmapQl)
import qualified HscTypes as GHC
import qualified Linker as GHC
import qualified Module as GHC
import qualified Name as GHC (nameModule_maybe)
import qualified Panic as GHC
import qualified SrcLoc as GHC
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

linkCoreExpr :: Bool -> GHC.HscEnv -> GHC.SrcSpan -> GHC.CoreExpr -> IO ()
linkCoreExpr verbose hsc_env src_span expr = do
  trace verbose "Asterius.Linker.LinkExpr.linkCoreExpr: initDynLinker"
  GHC.initDynLinker hsc_env
  trace verbose "Asterius.Linker.LinkExpr.linkCoreExpr: linkDependencies"
  ok <-
    GHC.modifyPLS $ \pls0 ->
      GHC.linkDependencies hsc_env pls0 src_span (coreExprModules expr)
  when (GHC.failed ok) $
    GHC.throwGhcExceptionIO $
    GHC.ProgramError "Asterius.Linker.linkCoreExpr: failed to link"
