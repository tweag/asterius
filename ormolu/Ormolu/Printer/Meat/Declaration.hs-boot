module Ormolu.Printer.Meat.Declaration
  ( p_hsDecls,
    p_hsDeclsRespectGrouping,
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common

p_hsDecls :: FamilyStyle -> [LHsDecl GhcPs] -> R ()
p_hsDeclsRespectGrouping :: FamilyStyle -> [LHsDecl GhcPs] -> R ()
