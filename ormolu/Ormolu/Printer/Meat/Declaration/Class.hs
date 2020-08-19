{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of type class declarations.
module Ormolu.Printer.Meat.Declaration.Class
  ( p_classDecl,
  )
where

import Class
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.List (sortOn)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Type

p_classDecl ::
  LHsContext GhcPs ->
  Located RdrName ->
  LHsQTyVars GhcPs ->
  LexicalFixity ->
  [Located (FunDep (Located RdrName))] ->
  [LSig GhcPs] ->
  LHsBinds GhcPs ->
  [LFamilyDecl GhcPs] ->
  [LTyFamDefltDecl GhcPs] ->
  [LDocDecl] ->
  R ()
p_classDecl ctx name HsQTvs {..} fixity fdeps csigs cdefs cats catdefs cdocs = do
  let variableSpans = getLoc <$> hsq_explicit
      signatureSpans = getLoc name : variableSpans
      dependencySpans = getLoc <$> fdeps
      combinedSpans = getLoc ctx : (signatureSpans ++ dependencySpans)
      -- GHC's AST does not necessarily store each kind of element in source
      -- location order. This happens because different declarations are stored
      -- in different lists. Consequently, to get all the declarations in proper
      -- order, they need to be manually sorted.
      sigs = (getLoc &&& fmap (SigD NoExtField)) <$> csigs
      vals = (getLoc &&& fmap (ValD NoExtField)) <$> toList cdefs
      tyFams = (getLoc &&& fmap (TyClD NoExtField . FamDecl NoExtField)) <$> cats
      docs = (getLoc &&& fmap (DocD NoExtField)) <$> cdocs
      tyFamDefs =
        ( getLoc &&& fmap (InstD NoExtField . TyFamInstD NoExtField)
        )
          <$> catdefs
      allDecls =
        snd <$> sortOn fst (sigs <> vals <> tyFams <> tyFamDefs <> docs)
  txt "class"
  switchLayout combinedSpans $ do
    breakpoint
    inci $ do
      p_classContext ctx
      switchLayout signatureSpans $
        p_infixDefHelper
          (isInfix fixity)
          True
          (p_rdrName name)
          (located' p_hsTyVarBndr <$> hsq_explicit)
      inci (p_classFundeps fdeps)
      unless (null allDecls) $ do
        breakpoint
        txt "where"
  unless (null allDecls) $ do
    breakpoint -- Ensure whitespace is added after where clause.
    inci (p_hsDeclsRespectGrouping Associated allDecls)
p_classDecl _ _ (XLHsQTyVars c) _ _ _ _ _ _ _ = noExtCon c

p_classContext :: LHsContext GhcPs -> R ()
p_classContext ctx = unless (null (unLoc ctx)) $ do
  located ctx p_hsContext
  space
  txt "=>"
  breakpoint

p_classFundeps :: [Located (FunDep (Located RdrName))] -> R ()
p_classFundeps fdeps = unless (null fdeps) $ do
  breakpoint
  txt "|"
  space
  inci $ sep commaDel (sitcc . located' p_funDep) fdeps

p_funDep :: FunDep (Located RdrName) -> R ()
p_funDep (before, after) = do
  sep space p_rdrName before
  space
  txt "->"
  space
  sep space p_rdrName after

----------------------------------------------------------------------------
-- Helpers

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False
