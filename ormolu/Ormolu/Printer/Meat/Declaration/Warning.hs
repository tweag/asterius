{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.Declaration.Warning
  ( p_warnDecls,
    p_moduleWarning,
  )
where

import BasicTypes
import Data.Foldable
import Data.Text (Text)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common

p_warnDecls :: WarnDecls GhcPs -> R ()
p_warnDecls (Warnings NoExtField _ warnings) =
  traverse_ (located' p_warnDecl) warnings
p_warnDecls (XWarnDecls x) = noExtCon x

p_warnDecl :: WarnDecl GhcPs -> R ()
p_warnDecl (Warning NoExtField functions warningTxt) =
  p_topLevelWarning functions warningTxt
p_warnDecl (XWarnDecl x) = noExtCon x

p_moduleWarning :: WarningTxt -> R ()
p_moduleWarning wtxt = do
  let (pragmaText, lits) = warningText wtxt
  inci $ pragma pragmaText $ inci $ p_lits lits

p_topLevelWarning :: [Located RdrName] -> WarningTxt -> R ()
p_topLevelWarning fnames wtxt = do
  let (pragmaText, lits) = warningText wtxt
  switchLayout (fmap getLoc fnames ++ fmap getLoc lits) $
    pragma pragmaText . inci $ do
      sep commaDel p_rdrName fnames
      breakpoint
      p_lits lits

warningText :: WarningTxt -> (Text, [Located StringLiteral])
warningText = \case
  WarningTxt _ lits -> ("WARNING", lits)
  DeprecatedTxt _ lits -> ("DEPRECATED", lits)

p_lits :: [Located StringLiteral] -> R ()
p_lits = \case
  [l] -> atom l
  ls -> brackets N $ sep commaDel atom ls
