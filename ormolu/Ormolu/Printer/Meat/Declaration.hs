{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Rendering of declarations.
module Ormolu.Printer.Meat.Declaration
  ( p_hsDecls,
    p_hsDeclsRespectGrouping,
  )
where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import GHC hiding (InlinePragma)
import OccName (occNameFS)
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Annotation
import Ormolu.Printer.Meat.Declaration.Class
import Ormolu.Printer.Meat.Declaration.Data
import Ormolu.Printer.Meat.Declaration.Default
import Ormolu.Printer.Meat.Declaration.Foreign
import Ormolu.Printer.Meat.Declaration.Instance
import Ormolu.Printer.Meat.Declaration.RoleAnnotation
import Ormolu.Printer.Meat.Declaration.Rule
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Splice
import Ormolu.Printer.Meat.Declaration.Type
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

data UserGrouping
  = -- | Always put newlines where we think they should be
    Disregard
  | -- | Respect user preferences regarding grouping
    Respect
  deriving (Eq, Show)

p_hsDecls :: FamilyStyle -> [LHsDecl GhcPs] -> R ()
p_hsDecls = p_hsDecls' Disregard

-- | Like 'p_hsDecls' but respects user choices regarding grouping. If the
-- user omits newlines between declarations, we also omit them in most
-- cases, except when said declarations have associated Haddocks.
--
-- Does some normalization (compress subsequent newlines into a single one)
p_hsDeclsRespectGrouping :: FamilyStyle -> [LHsDecl GhcPs] -> R ()
p_hsDeclsRespectGrouping = p_hsDecls' Respect

p_hsDecls' :: UserGrouping -> FamilyStyle -> [LHsDecl GhcPs] -> R ()
p_hsDecls' grouping style decls =
  sepSemi id $
    -- Return a list of rendered declarations, adding a newline to separate
    -- groups.
    case groupDecls decls of
      [] -> []
      (x : xs) -> renderGroup x ++ concat (zipWith renderGroupWithPrev (x : xs) xs)
  where
    renderGroup = NE.toList . fmap (located' $ dontUseBraces . p_hsDecl style)
    renderGroupWithPrev prev curr =
      -- We can omit a blank line when the user didn't add one, but we must
      -- ensure we always add blank lines around documented declarations
      case grouping of
        Disregard ->
          breakpoint : renderGroup curr
        Respect ->
          if separatedByBlankNE getLoc prev curr
            || isDocumented prev
            || isDocumented curr
            then breakpoint : renderGroup curr
            else renderGroup curr

-- | Is a declaration group documented?
isDocumented :: NonEmpty (LHsDecl GhcPs) -> Bool
isDocumented = any (isHaddock . unLoc)
  where
    isHaddock DocNext = True
    isHaddock DocPrev = True
    isHaddock _ = False

-- | Group relevant declarations together.
groupDecls :: [LHsDecl GhcPs] -> [NonEmpty (LHsDecl GhcPs)]
groupDecls [] = []
groupDecls (l@(L _ DocNext) : xs) =
  -- If the first element is a doc string for next element, just include it
  -- in the next block:
  case groupDecls xs of
    [] -> [l :| []]
    (x : xs') -> (l <| x) : xs'
groupDecls (header : xs) =
  let (grp, rest) = flip span (zip (header : xs) xs) $ \(previous, current) ->
        let relevantToHdr = groupedDecls header current
            relevantToPrev = groupedDecls previous current
            isDeclSeries = declSeries previous current
         in isDeclSeries || relevantToHdr || relevantToPrev
   in (header :| map snd grp) : groupDecls (map snd rest)

p_hsDecl :: FamilyStyle -> HsDecl GhcPs -> R ()
p_hsDecl style = \case
  TyClD NoExtField x -> p_tyClDecl style x
  ValD NoExtField x -> p_valDecl x
  SigD NoExtField x -> p_sigDecl x
  InstD NoExtField x -> p_instDecl style x
  DerivD NoExtField x -> p_derivDecl x
  DefD NoExtField x -> p_defaultDecl x
  ForD NoExtField x -> p_foreignDecl x
  WarningD NoExtField x -> p_warnDecls x
  AnnD NoExtField x -> p_annDecl x
  RuleD NoExtField x -> p_ruleDecls x
  SpliceD NoExtField x -> p_spliceDecl x
  DocD NoExtField docDecl ->
    case docDecl of
      DocCommentNext str -> p_hsDocString Pipe False (noLoc str)
      DocCommentPrev str -> p_hsDocString Caret False (noLoc str)
      DocCommentNamed name str -> p_hsDocString (Named name) False (noLoc str)
      DocGroup n str -> p_hsDocString (Asterisk n) False (noLoc str)
  RoleAnnotD NoExtField x -> p_roleAnnot x
  KindSigD NoExtField s -> p_standaloneKindSig s
  XHsDecl x -> noExtCon x

p_tyClDecl :: FamilyStyle -> TyClDecl GhcPs -> R ()
p_tyClDecl style = \case
  FamDecl NoExtField x -> p_famDecl style x
  SynDecl {..} -> p_synDecl tcdLName tcdFixity tcdTyVars tcdRhs
  DataDecl {..} ->
    p_dataDecl
      Associated
      tcdLName
      (tyVarsToTypes tcdTyVars)
      tcdFixity
      tcdDataDefn
  ClassDecl {..} ->
    p_classDecl
      tcdCtxt
      tcdLName
      tcdTyVars
      tcdFixity
      tcdFDs
      tcdSigs
      tcdMeths
      tcdATs
      tcdATDefs
      tcdDocs
  XTyClDecl x -> noExtCon x

p_instDecl :: FamilyStyle -> InstDecl GhcPs -> R ()
p_instDecl style = \case
  ClsInstD NoExtField x -> p_clsInstDecl x
  TyFamInstD NoExtField x -> p_tyFamInstDecl style x
  DataFamInstD NoExtField x -> p_dataFamInstDecl style x
  XInstDecl x -> noExtCon x

p_derivDecl :: DerivDecl GhcPs -> R ()
p_derivDecl = \case
  d@DerivDecl {} -> p_standaloneDerivDecl d
  XDerivDecl x -> noExtCon x

-- | Determine if these declarations should be grouped together.
groupedDecls ::
  LHsDecl GhcPs ->
  LHsDecl GhcPs ->
  Bool
groupedDecls (L l_x x') (L l_y y') =
  case (x', y') of
    (TypeSignature ns, FunctionBody ns') -> ns `intersects` ns'
    (TypeSignature ns, DefaultSignature ns') -> ns `intersects` ns'
    (DefaultSignature ns, TypeSignature ns') -> ns `intersects` ns'
    (DefaultSignature ns, FunctionBody ns') -> ns `intersects` ns'
    (x, FunctionBody ns) | Just ns' <- isPragma x -> ns `intersects` ns'
    (FunctionBody ns, x) | Just ns' <- isPragma x -> ns `intersects` ns'
    (x, DataDeclaration n) | Just ns <- isPragma x -> n `elem` ns
    (DataDeclaration n, x)
      | Just ns <- isPragma x ->
        let f = occNameFS . rdrNameOcc in f n `elem` map f ns
    (x, y)
      | Just ns <- isPragma x,
        Just ns' <- isPragma y ->
        ns `intersects` ns'
    (x, TypeSignature ns) | Just ns' <- isPragma x -> ns `intersects` ns'
    (TypeSignature ns, x) | Just ns' <- isPragma x -> ns `intersects` ns'
    (PatternSignature ns, Pattern n) -> n `elem` ns
    (KindSignature n, DataDeclaration n') -> n == n'
    (KindSignature n, ClassDeclaration n') -> n == n'
    (KindSignature n, FamilyDeclaration n') -> n == n'
    -- Special case for TH splices, we look at locations
    (Splice, Splice) -> not (separatedByBlank id l_x l_y)
    -- This looks only at Haddocks, normal comments are handled elsewhere
    (DocNext, _) -> True
    (_, DocPrev) -> True
    _ -> False

-- | Detect declaration series that should not have blanks between them.
declSeries ::
  LHsDecl GhcPs ->
  LHsDecl GhcPs ->
  Bool
declSeries (L _ x) (L _ y) =
  case (x, y) of
    ( SigD NoExtField (TypeSig NoExtField _ _),
      SigD NoExtField (TypeSig NoExtField _ _)
      ) -> True
    _ -> False

intersects :: Ord a => [a] -> [a] -> Bool
intersects a b = go (sort a) (sort b)
  where
    go :: Ord a => [a] -> [a] -> Bool
    go _ [] = False
    go [] _ = False
    go (x : xs) (y : ys)
      | x < y = go xs (y : ys)
      | x > y = go (x : xs) ys
      | otherwise = True

isPragma ::
  HsDecl GhcPs ->
  Maybe [RdrName]
isPragma = \case
  InlinePragma n -> Just [n]
  SpecializePragma n -> Just [n]
  SCCPragma n -> Just [n]
  AnnTypePragma n -> Just [n]
  AnnValuePragma n -> Just [n]
  WarningPragma n -> Just n
  _ -> Nothing

-- Declarations that do not refer to names

pattern Splice :: HsDecl GhcPs
pattern Splice <- SpliceD NoExtField (SpliceDecl NoExtField _ _)

-- Declarations referring to a single name

pattern
  InlinePragma,
  SpecializePragma,
  SCCPragma,
  AnnTypePragma,
  AnnValuePragma,
  Pattern,
  DataDeclaration,
  ClassDeclaration,
  KindSignature,
  FamilyDeclaration ::
    RdrName -> HsDecl GhcPs
pattern InlinePragma n <- SigD NoExtField (InlineSig NoExtField (L _ n) _)
pattern SpecializePragma n <- SigD NoExtField (SpecSig NoExtField (L _ n) _ _)
pattern SCCPragma n <- SigD NoExtField (SCCFunSig NoExtField _ (L _ n) _)
pattern AnnTypePragma n <- AnnD NoExtField (HsAnnotation NoExtField _ (TypeAnnProvenance (L _ n)) _)
pattern AnnValuePragma n <- AnnD NoExtField (HsAnnotation NoExtField _ (ValueAnnProvenance (L _ n)) _)
pattern Pattern n <- ValD NoExtField (PatSynBind NoExtField (PSB _ (L _ n) _ _ _))
pattern DataDeclaration n <- TyClD NoExtField (DataDecl NoExtField (L _ n) _ _ _)
pattern ClassDeclaration n <- TyClD NoExtField (ClassDecl NoExtField _ (L _ n) _ _ _ _ _ _ _ _)
pattern KindSignature n <- KindSigD NoExtField (StandaloneKindSig NoExtField (L _ n) _)
pattern FamilyDeclaration n <- TyClD NoExtField (FamDecl NoExtField (FamilyDecl NoExtField _ (L _ n) _ _ _ _))

-- Declarations which can refer to multiple names

pattern
  TypeSignature,
  DefaultSignature,
  FunctionBody,
  PatternSignature,
  WarningPragma ::
    [RdrName] -> HsDecl GhcPs
pattern TypeSignature n <- (sigRdrNames -> Just n)
pattern DefaultSignature n <- (defSigRdrNames -> Just n)
pattern FunctionBody n <- (funRdrNames -> Just n)
pattern PatternSignature n <- (patSigRdrNames -> Just n)
pattern WarningPragma n <- (warnSigRdrNames -> Just n)

pattern DocNext, DocPrev :: HsDecl GhcPs
pattern DocNext <- (DocD NoExtField (DocCommentNext _))
pattern DocPrev <- (DocD NoExtField (DocCommentPrev _))

sigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
sigRdrNames (SigD NoExtField (TypeSig NoExtField ns _)) = Just $ map unLoc ns
sigRdrNames (SigD NoExtField (ClassOpSig NoExtField _ ns _)) = Just $ map unLoc ns
sigRdrNames (SigD NoExtField (PatSynSig NoExtField ns _)) = Just $ map unLoc ns
sigRdrNames _ = Nothing

defSigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
defSigRdrNames (SigD NoExtField (ClassOpSig NoExtField True ns _)) = Just $ map unLoc ns
defSigRdrNames _ = Nothing

funRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
funRdrNames (ValD NoExtField (FunBind NoExtField (L _ n) _ _ _)) = Just [n]
funRdrNames (ValD NoExtField (PatBind NoExtField (L _ n) _ _)) = Just $ patBindNames n
funRdrNames _ = Nothing

patSigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
patSigRdrNames (SigD NoExtField (PatSynSig NoExtField ns _)) = Just $ map unLoc ns
patSigRdrNames _ = Nothing

warnSigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
warnSigRdrNames (WarningD NoExtField (Warnings NoExtField _ ws)) = Just $
  flip concatMap ws $ \case
    L _ (Warning NoExtField ns _) -> map unLoc ns
    L _ (XWarnDecl x) -> noExtCon x
warnSigRdrNames _ = Nothing

patBindNames :: Pat GhcPs -> [RdrName]
patBindNames (TuplePat NoExtField ps _) = concatMap (patBindNames . unLoc) ps
patBindNames (VarPat NoExtField (L _ n)) = [n]
patBindNames (WildPat NoExtField) = []
patBindNames (LazyPat NoExtField (L _ p)) = patBindNames p
patBindNames (BangPat NoExtField (L _ p)) = patBindNames p
patBindNames (ParPat NoExtField (L _ p)) = patBindNames p
patBindNames (ListPat NoExtField ps) = concatMap (patBindNames . unLoc) ps
patBindNames (AsPat NoExtField (L _ n) (L _ p)) = n : patBindNames p
patBindNames (SumPat NoExtField (L _ p) _ _) = patBindNames p
patBindNames (ViewPat NoExtField _ (L _ p)) = patBindNames p
patBindNames (SplicePat NoExtField _) = []
patBindNames (LitPat NoExtField _) = []
patBindNames (SigPat _ (L _ p) _) = patBindNames p
patBindNames (NPat NoExtField _ _ _) = []
patBindNames (NPlusKPat NoExtField (L _ n) _ _ _ _) = [n]
patBindNames (ConPatIn _ d) = concatMap (patBindNames . unLoc) (hsConPatArgs d)
patBindNames ConPatOut {} = notImplemented "ConPatOut" -- created by renamer
patBindNames (CoPat NoExtField _ p _) = patBindNames p
patBindNames (XPat x) = noExtCon x
