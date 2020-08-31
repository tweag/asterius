module Asterius.Internals.Name
  ( idClosureSymbol,
  )
where

import Asterius.Types (EntitySymbol)
import Asterius.TypesConv
import qualified CLabel as GHC
import Data.String
import qualified DynFlags as GHC
import qualified Id as GHC

idClosureSymbol :: GHC.DynFlags -> GHC.Id -> EntitySymbol
idClosureSymbol dflags n =
  fromString $
    asmPpr dflags $
      GHC.mkClosureLabel
        (GHC.idName n)
        (GHC.idCafInfo n)
