{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.Declaration.Rule
  ( p_ruleDecls,
  )
where

import BasicTypes
import Control.Monad (unless)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Type

p_ruleDecls :: RuleDecls GhcPs -> R ()
p_ruleDecls = \case
  HsRules NoExtField _ xs ->
    pragma "RULES" $
      sep breakpoint (sitcc . located' p_ruleDecl) xs
  XRuleDecls x -> noExtCon x

p_ruleDecl :: RuleDecl GhcPs -> R ()
p_ruleDecl = \case
  HsRule NoExtField ruleName activation tyvars ruleBndrs lhs rhs -> do
    located ruleName p_ruleName
    space
    p_activation activation
    space
    case tyvars of
      Nothing -> return ()
      Just xs -> do
        p_forallBndrs ForallInvis p_hsTyVarBndr xs
        space
    -- It appears that there is no way to tell if there was an empty forall
    -- in the input or no forall at all. We do not want to add redundant
    -- foralls, so let's just skip the empty ones.
    unless (null ruleBndrs) $
      p_forallBndrs ForallInvis p_ruleBndr ruleBndrs
    breakpoint
    inci $ do
      located lhs p_hsExpr
      space
      equals
      inci $ do
        breakpoint
        located rhs p_hsExpr
  XRuleDecl x -> noExtCon x

p_ruleName :: (SourceText, RuleName) -> R ()
p_ruleName (_, name) = atom $ (HsString NoSourceText name :: HsLit GhcPs)

p_ruleBndr :: RuleBndr GhcPs -> R ()
p_ruleBndr = \case
  RuleBndr NoExtField x -> p_rdrName x
  RuleBndrSig NoExtField x hswc -> parens N $ do
    p_rdrName x
    p_typeAscription hswc
  XRuleBndr x -> noExtCon x
