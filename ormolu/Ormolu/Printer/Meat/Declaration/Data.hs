{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Renedring of data type declarations.
module Ormolu.Printer.Meat.Declaration.Data
  ( p_dataDecl,
  )
where

import Control.Monad
import Data.Maybe (isJust, maybeToList)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_dataDecl ::
  -- | Whether to format as data family
  FamilyStyle ->
  -- | Type constructor
  Located RdrName ->
  -- | Type patterns
  [LHsType GhcPs] ->
  -- | Lexical fixity
  LexicalFixity ->
  -- | Data definition
  HsDataDefn GhcPs ->
  R ()
p_dataDecl style name tpats fixity HsDataDefn {..} = do
  txt $ case dd_ND of
    NewType -> "newtype"
    DataType -> "data"
  txt $ case style of
    Associated -> mempty
    Free -> " instance"
  let constructorSpans = getLoc name : fmap getLoc tpats
  switchLayout constructorSpans $ do
    breakpoint
    inci $
      p_infixDefHelper
        (isInfix fixity)
        True
        (p_rdrName name)
        (located' p_hsType <$> tpats)
  case dd_kindSig of
    Nothing -> return ()
    Just k -> do
      space
      txt "::"
      space
      located k p_hsType
  let gadt = isJust dd_kindSig || any (isGadt . unLoc) dd_cons
  unless (null dd_cons) $
    if gadt
      then inci $ do
        switchLayout constructorSpans $ do
          breakpoint
          txt "where"
        breakpoint
        sepSemi (located' (p_conDecl False)) dd_cons
      else switchLayout (getLoc name : (getLoc <$> dd_cons)) . inci $ do
        let singleConstRec = isSingleConstRec dd_cons
        if singleConstRec
          then space
          else
            if hasHaddocks dd_cons
              then newline
              else breakpoint
        equals
        space
        layout <- getLayout
        let s =
              if layout == MultiLine || hasHaddocks dd_cons
                then newline >> txt "|" >> space
                else space >> txt "|" >> space
            sitcc' =
              if singleConstRec
                then id
                else sitcc
        sep s (sitcc' . located' (p_conDecl singleConstRec)) dd_cons
  unless (null $ unLoc dd_derivs) breakpoint
  inci . located dd_derivs $ \xs ->
    sep newline (located' p_hsDerivingClause) xs
p_dataDecl _ _ _ _ (XHsDataDefn x) = noExtCon x

p_conDecl ::
  Bool ->
  ConDecl GhcPs ->
  R ()
p_conDecl singleConstRec = \case
  ConDeclGADT {..} -> do
    mapM_ (p_hsDocString Pipe True) con_doc
    let conDeclSpn =
          fmap getLoc con_names
            <> [getLoc con_forall]
            <> conTyVarsSpans con_qvars
            <> maybeToList (fmap getLoc con_mb_cxt)
            <> conArgsSpans con_args
    switchLayout conDeclSpn $ do
      case con_names of
        [] -> return ()
        (c : cs) -> do
          p_rdrName c
          unless (null cs) . inci $ do
            commaDel
            sep commaDel p_rdrName cs
      inci $ do
        space
        txt "::"
        let interArgBreak =
              if hasDocStrings (unLoc con_res_ty)
                then newline
                else breakpoint
        interArgBreak
        when (unLoc con_forall) $ do
          p_forallBndrs ForallInvis p_hsTyVarBndr (hsq_explicit con_qvars)
          interArgBreak
        forM_ con_mb_cxt p_lhsContext
        case con_args of
          PrefixCon xs -> do
            sep breakpoint (located' p_hsType) xs
            unless (null xs) $ do
              space
              txt "->"
              breakpoint
          RecCon l -> do
            located l p_conDeclFields
            unless (null $ unLoc l) $ do
              space
              txt "->"
              breakpoint
          InfixCon _ _ -> notImplemented "InfixCon"
        p_hsType (unLoc con_res_ty)
  ConDeclH98 {..} -> do
    mapM_ (p_hsDocString Pipe True) con_doc
    let conDeclWithContextSpn =
          [getLoc con_forall]
            <> fmap getLoc con_ex_tvs
            <> maybeToList (fmap getLoc con_mb_cxt)
            <> conDeclSpn
        conDeclSpn =
          getLoc con_name : conArgsSpans con_args
    switchLayout conDeclWithContextSpn $ do
      when (unLoc con_forall) $ do
        p_forallBndrs ForallInvis p_hsTyVarBndr con_ex_tvs
        breakpoint
      forM_ con_mb_cxt p_lhsContext
      switchLayout conDeclSpn $ case con_args of
        PrefixCon xs -> do
          p_rdrName con_name
          unless (null xs) breakpoint
          inci . sitcc $ sep breakpoint (sitcc . located' p_hsTypePostDoc) xs
        RecCon l -> do
          p_rdrName con_name
          breakpoint
          inciIf (not singleConstRec) (located l p_conDeclFields)
        InfixCon x y -> do
          located x p_hsType
          breakpoint
          inci $ do
            p_rdrName con_name
            space
            located y p_hsType
  XConDecl x -> noExtCon x

conArgsSpans :: HsConDeclDetails GhcPs -> [SrcSpan]
conArgsSpans = \case
  PrefixCon xs ->
    getLoc <$> xs
  RecCon l ->
    [getLoc l]
  InfixCon x y ->
    [getLoc x, getLoc y]

conTyVarsSpans :: LHsQTyVars GhcPs -> [SrcSpan]
conTyVarsSpans = \case
  HsQTvs {..} -> getLoc <$> hsq_explicit
  XLHsQTyVars x -> noExtCon x

p_lhsContext ::
  LHsContext GhcPs ->
  R ()
p_lhsContext = \case
  L _ [] -> pure ()
  ctx -> do
    located ctx p_hsContext
    space
    txt "=>"
    breakpoint

isGadt :: ConDecl GhcPs -> Bool
isGadt = \case
  ConDeclGADT {} -> True
  ConDeclH98 {} -> False
  XConDecl {} -> False

p_hsDerivingClause ::
  HsDerivingClause GhcPs ->
  R ()
p_hsDerivingClause HsDerivingClause {..} = do
  txt "deriving"
  let derivingWhat = located deriv_clause_tys $ \case
        [] -> txt "()"
        xs ->
          parens N $
            sep
              commaDel
              (sitcc . located' p_hsType . hsib_body)
              xs
  space
  case deriv_clause_strategy of
    Nothing -> do
      breakpoint
      inci derivingWhat
    Just (L _ a) -> case a of
      StockStrategy -> do
        txt "stock"
        breakpoint
        inci derivingWhat
      AnyclassStrategy -> do
        txt "anyclass"
        breakpoint
        inci derivingWhat
      NewtypeStrategy -> do
        txt "newtype"
        breakpoint
        inci derivingWhat
      ViaStrategy HsIB {..} -> do
        breakpoint
        inci $ do
          derivingWhat
          breakpoint
          txt "via"
          space
          located hsib_body p_hsType
      ViaStrategy (XHsImplicitBndrs x) ->
        noExtCon x
p_hsDerivingClause (XHsDerivingClause x) = noExtCon x

----------------------------------------------------------------------------
-- Helpers

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False

isSingleConstRec :: [LConDecl GhcPs] -> Bool
isSingleConstRec [(L _ ConDeclH98 {..})] =
  case con_args of
    RecCon _ -> True
    _ -> False
isSingleConstRec _ = False

hasHaddocks :: [LConDecl GhcPs] -> Bool
hasHaddocks = any (f . unLoc)
  where
    f ConDeclH98 {..} = isJust con_doc
    f _ = False
