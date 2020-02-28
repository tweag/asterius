module Asterius.Internals.Name
  ( fakeClosureSymbol,
    idClosureSymbol,
  )
where

import Asterius.Types (AsteriusEntitySymbol)
import Asterius.TypesConv
import qualified CLabel as GHC
import Data.String
import qualified DynFlags as GHC
import qualified FastString as GHC
import qualified Id as GHC
import qualified IdInfo as GHC
import qualified Module as GHC
import qualified Name as GHC
import qualified Packages as GHC
import qualified SrcLoc as GHC
import qualified Unique as GHC

fakeName ::
  GHC.DynFlags ->
  GHC.PackageName ->
  GHC.ModuleName ->
  GHC.OccName ->
  GHC.Name
fakeName dflags pkg_name mod_name occ_name = name
  where
    dummy_uniq = GHC.mkUniqueGrimily 0
    Just comp_id = GHC.lookupPackageName dflags pkg_name
    inst_unit_id = GHC.componentIdToInstalledUnitId comp_id
    Just pkg_conf = GHC.lookupInstalledPackage dflags inst_unit_id
    unit_id = GHC.packageConfigId pkg_conf
    m = GHC.mkModule unit_id mod_name
    name = GHC.mkExternalName dummy_uniq m occ_name GHC.noSrcSpan

fakeClosureSymbol ::
  GHC.DynFlags -> String -> String -> String -> AsteriusEntitySymbol
fakeClosureSymbol dflags pkg_name mod_name occ_name = sym
  where
    name =
      fakeName
        dflags
        (GHC.PackageName (GHC.mkFastString pkg_name))
        (GHC.mkModuleName mod_name)
        (GHC.mkVarOcc occ_name)
    clbl = GHC.mkClosureLabel name GHC.MayHaveCafRefs
    sym = fromString $ asmPpr dflags clbl

idClosureSymbol :: GHC.DynFlags -> GHC.Id -> AsteriusEntitySymbol
idClosureSymbol dflags n =
  fromString $ asmPpr dflags $
    GHC.mkClosureLabel
      (GHC.idName n)
      (GHC.idCafInfo n)
