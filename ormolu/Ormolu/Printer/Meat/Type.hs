{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of types.
module Ormolu.Printer.Meat.Type
  ( p_hsType,
    p_hsTypePostDoc,
    hasDocStrings,
    p_hsContext,
    p_hsTyVarBndr,
    p_forallBndrs,
    p_conDeclFields,
    tyVarsToTypes,
  )
where

import Data.Data (Data)
import GHC hiding (isPromoted)
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.Value (p_hsSplice, p_stringLit)
import Ormolu.Printer.Operators
import Ormolu.Utils

p_hsType :: HsType GhcPs -> R ()
p_hsType t = p_hsType' (hasDocStrings t) PipeStyle t

p_hsTypePostDoc :: HsType GhcPs -> R ()
p_hsTypePostDoc t = p_hsType' (hasDocStrings t) CaretStyle t

-- | How to render Haddocks associated with a type.
data TypeDocStyle
  = PipeStyle
  | CaretStyle

p_hsType' :: Bool -> TypeDocStyle -> HsType GhcPs -> R ()
p_hsType' multilineArgs docStyle = \case
  HsForAllTy NoExtField visibility bndrs t -> do
    p_forallBndrs visibility p_hsTyVarBndr bndrs
    interArgBreak
    p_hsTypeR (unLoc t)
  HsQualTy NoExtField qs t -> do
    located qs p_hsContext
    space
    txt "=>"
    interArgBreak
    case unLoc t of
      HsQualTy {} -> p_hsTypeR (unLoc t)
      HsFunTy {} -> p_hsTypeR (unLoc t)
      _ -> located t p_hsTypeR
  HsTyVar NoExtField p n -> do
    case p of
      IsPromoted -> do
        txt "'"
        case showOutputable (unLoc n) of
          _ : '\'' : _ -> space
          _ -> return ()
      NotPromoted -> return ()
    p_rdrName n
  HsAppTy NoExtField f x -> do
    let -- In order to format type applications with multiple parameters
        -- nicer, traverse the AST to gather the function and all the
        -- parameters together.
        gatherArgs f' knownArgs =
          case f' of
            L _ (HsAppTy _ l r) -> gatherArgs l (r : knownArgs)
            _ -> (f', knownArgs)
        (func, args) = gatherArgs f [x]
    switchLayout (getLoc f : fmap getLoc args) . sitcc $ do
      located func p_hsType
      breakpoint
      inci $
        sep breakpoint (located' p_hsType) args
  HsAppKindTy _ ty kd -> sitcc $ do
    -- The first argument is the location of the "@..." part. Not 100% sure,
    -- but I think we can ignore it as long as we use 'located' on both the
    -- type and the kind.
    located ty p_hsType
    breakpoint
    inci $ do
      txt "@"
      located kd p_hsType
  HsFunTy NoExtField x y@(L _ y') -> do
    located x p_hsType
    space
    txt "->"
    interArgBreak
    case y' of
      HsFunTy {} -> p_hsTypeR y'
      _ -> located y p_hsTypeR
  HsListTy NoExtField t ->
    located t (brackets N . p_hsType)
  HsTupleTy NoExtField tsort xs ->
    let parens' =
          case tsort of
            HsUnboxedTuple -> parensHash N
            HsBoxedTuple -> parens N
            HsConstraintTuple -> parens N
            HsBoxedOrConstraintTuple -> parens N
     in parens' $ sep commaDel (sitcc . located' p_hsType) xs
  HsSumTy NoExtField xs ->
    parensHash N $
      sep (txt "|" >> breakpoint) (sitcc . located' p_hsType) xs
  HsOpTy NoExtField x op y ->
    sitcc $
      let opTree = OpBranch (tyOpTree x) op (tyOpTree y)
       in p_tyOpTree (reassociateOpTree Just opTree)
  HsParTy NoExtField t ->
    parens N (located t p_hsType)
  HsIParamTy NoExtField n t -> sitcc $ do
    located n atom
    space
    txt "::"
    breakpoint
    inci (located t p_hsType)
  HsStarTy NoExtField _ -> txt "*"
  HsKindSig NoExtField t k -> sitcc $ do
    located t p_hsType
    space
    txt "::"
    breakpoint
    inci (located k p_hsType)
  HsSpliceTy NoExtField splice -> p_hsSplice splice
  HsDocTy NoExtField t str ->
    case docStyle of
      PipeStyle -> do
        p_hsDocString Pipe True str
        located t p_hsType
      CaretStyle -> do
        located t p_hsType
        newline
        p_hsDocString Caret False str
  HsBangTy NoExtField (HsSrcBang _ u s) t -> do
    case u of
      SrcUnpack -> txt "{-# UNPACK #-}" >> space
      SrcNoUnpack -> txt "{-# NOUNPACK #-}" >> space
      NoSrcUnpack -> return ()
    case s of
      SrcLazy -> txt "~"
      SrcStrict -> txt "!"
      NoSrcStrict -> return ()
    located t p_hsType
  HsRecTy NoExtField fields ->
    p_conDeclFields fields
  HsExplicitListTy NoExtField p xs -> do
    case p of
      IsPromoted -> txt "'"
      NotPromoted -> return ()
    brackets N $ do
      -- If both this list itself and the first element is promoted,
      -- we need to put a space in between or it fails to parse.
      case (p, xs) of
        (IsPromoted, L _ t : _) | isPromoted t -> space
        _ -> return ()
      sep commaDel (sitcc . located' p_hsType) xs
  HsExplicitTupleTy NoExtField xs -> do
    txt "'"
    parens N $ do
      case xs of
        L _ t : _ | isPromoted t -> space
        _ -> return ()
      sep commaDel (located' p_hsType) xs
  HsTyLit NoExtField t ->
    case t of
      HsStrTy (SourceText s) _ -> p_stringLit s
      a -> atom a
  HsWildCardTy NoExtField -> txt "_"
  XHsType (NHsCoreTy t) -> atom t
  where
    isPromoted = \case
      HsTyVar _ IsPromoted _ -> True
      HsExplicitTupleTy {} -> True
      HsExplicitListTy {} -> True
      _ -> False
    interArgBreak =
      if multilineArgs
        then newline
        else breakpoint
    p_hsTypeR = p_hsType' multilineArgs docStyle

-- | Return 'True' if at least one argument in 'HsType' has a doc string
-- attached to it.
hasDocStrings :: HsType GhcPs -> Bool
hasDocStrings = \case
  HsDocTy {} -> True
  HsFunTy _ (L _ x) (L _ y) -> hasDocStrings x || hasDocStrings y
  _ -> False

p_hsContext :: HsContext GhcPs -> R ()
p_hsContext = \case
  [] -> txt "()"
  [x] -> located x p_hsType
  xs -> parens N $ sep commaDel (sitcc . located' p_hsType) xs

p_hsTyVarBndr :: HsTyVarBndr GhcPs -> R ()
p_hsTyVarBndr = \case
  UserTyVar NoExtField x ->
    p_rdrName x
  KindedTyVar NoExtField l k -> parens N $ do
    located l atom
    space
    txt "::"
    breakpoint
    inci (located k p_hsType)
  XTyVarBndr x -> noExtCon x

-- | Render several @forall@-ed variables.
p_forallBndrs :: Data a => ForallVisFlag -> (a -> R ()) -> [Located a] -> R ()
p_forallBndrs ForallInvis _ [] = txt "forall."
p_forallBndrs ForallVis _ [] = txt "forall ->"
p_forallBndrs vis p tyvars =
  switchLayout (getLoc <$> tyvars) $ do
    txt "forall"
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p) tyvars
      case vis of
        ForallInvis -> txt "."
        ForallVis -> space >> txt "->"

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields xs =
  braces N $ sep commaDel (sitcc . located' p_conDeclField) xs

p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = do
  mapM_ (p_hsDocString Pipe True) cd_fld_doc
  sitcc $
    sep
      commaDel
      (located' (p_rdrName . rdrNameFieldOcc))
      cd_fld_names
  space
  txt "::"
  breakpoint
  sitcc . inci $ p_hsType (unLoc cd_fld_type)
p_conDeclField (XConDeclField x) = noExtCon x

tyOpTree :: LHsType GhcPs -> OpTree (LHsType GhcPs) (Located RdrName)
tyOpTree (L _ (HsOpTy NoExtField l op r)) =
  OpBranch (tyOpTree l) op (tyOpTree r)
tyOpTree n = OpNode n

p_tyOpTree :: OpTree (LHsType GhcPs) (Located RdrName) -> R ()
p_tyOpTree (OpNode n) = located n p_hsType
p_tyOpTree (OpBranch l op r) = do
  switchLayout [opTreeLoc l] $
    p_tyOpTree l
  breakpoint
  inci . switchLayout [opTreeLoc r] $ do
    p_rdrName op
    space
    p_tyOpTree r

----------------------------------------------------------------------------
-- Conversion functions

tyVarsToTypes :: LHsQTyVars GhcPs -> [LHsType GhcPs]
tyVarsToTypes = \case
  HsQTvs {..} -> fmap tyVarToType <$> hsq_explicit
  XLHsQTyVars x -> noExtCon x

tyVarToType :: HsTyVarBndr GhcPs -> HsType GhcPs
tyVarToType = \case
  UserTyVar NoExtField tvar -> HsTyVar NoExtField NotPromoted tvar
  KindedTyVar NoExtField tvar kind ->
    -- Note: we always add parentheses because for whatever reason GHC does
    -- not use HsParTy for left-hand sides of declarations. Please see
    -- <https://gitlab.haskell.org/ghc/ghc/issues/17404>. This is fine as
    -- long as 'tyVarToType' does not get applied to right-hand sides of
    -- declarations.
    HsParTy NoExtField . noLoc $
      HsKindSig NoExtField (noLoc (HsTyVar NoExtField NotPromoted tvar)) kind
  XTyVarBndr x -> noExtCon x
