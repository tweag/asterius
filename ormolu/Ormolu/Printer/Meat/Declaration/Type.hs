{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of type synonym declarations.
module Ormolu.Printer.Meat.Declaration.Type
  ( p_synDecl,
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type

p_synDecl ::
  -- | Type constructor
  Located RdrName ->
  -- | Fixity
  LexicalFixity ->
  -- | Type variables
  LHsQTyVars GhcPs ->
  -- | RHS of type declaration
  LHsType GhcPs ->
  R ()
p_synDecl name fixity HsQTvs {..} t = do
  txt "type"
  space
  switchLayout (getLoc name : map getLoc hsq_explicit) $
    p_infixDefHelper
      (case fixity of Infix -> True; _ -> False)
      True
      (p_rdrName name)
      (map (located' p_hsTyVarBndr) hsq_explicit)
  space
  equals
  if hasDocStrings (unLoc t)
    then newline
    else breakpoint
  inci (located t p_hsType)
p_synDecl _ _ (XLHsQTyVars x) _ = noExtCon x
