{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering of commonly useful bits.
module Ormolu.Printer.Meat.Common
  ( FamilyStyle (..),
    p_hsmodName,
    p_ieWrappedName,
    p_rdrName,
    doesNotNeedExtraParens,
    p_qualName,
    p_infixDefHelper,
    p_hsDocString,
    p_hsDocName,
  )
where

import Control.Monad
import Data.List (isPrefixOf)
import qualified Data.Text as T
import GHC hiding (GhcPs, IE)
import Name (nameStableString)
import OccName (OccName (..))
import Ormolu.Printer.Combinators
import Ormolu.Utils

-- | Data and type family style.
data FamilyStyle
  = -- | Declarations in type classes
    Associated
  | -- | Top-level declarations
    Free

p_hsmodName :: ModuleName -> R ()
p_hsmodName mname = do
  txt "module"
  space
  atom mname

p_ieWrappedName :: IEWrappedName RdrName -> R ()
p_ieWrappedName = \case
  IEName x -> p_rdrName x
  IEPattern x -> do
    txt "pattern"
    space
    p_rdrName x
  IEType x -> do
    txt "type"
    space
    p_rdrName x

-- | Render a @'Located' 'RdrName'@.
p_rdrName :: Located RdrName -> R ()
p_rdrName l@(L spn _) = located l $ \x -> do
  ids <- getAnns spn
  let backticksWrapper =
        if AnnBackquote `elem` ids
          then backticks
          else id
      parensWrapper =
        if AnnOpenP `elem` ids
          then parens N
          else id
      singleQuoteWrapper =
        if AnnSimpleQuote `elem` ids
          then \y -> do
            txt "'"
            y
          else id
      m =
        case x of
          Unqual occName ->
            atom occName
          Qual mname occName ->
            p_qualName mname occName
          Orig _ occName ->
            -- This is used when GHC generates code that will be fed into
            -- the renamer (e.g. from deriving clauses), but where we want
            -- to say that something comes from given module which is not
            -- specified in the source code, e.g. @Prelude.map@.
            --
            -- My current understanding is that the provided module name
            -- serves no purpose for us and can be safely ignored.
            atom occName
          Exact name ->
            atom name
      m' = backticksWrapper (singleQuoteWrapper m)
  if doesNotNeedExtraParens x
    then m'
    else parensWrapper m'

-- | Whether given name should not have parentheses around it. This is used
-- to detect e.g. tuples for which annotations will indicate parentheses,
-- but the parentheses are already part of the symbol, so no extra layer of
-- parentheses should be added. It also detects the [] literal.
doesNotNeedExtraParens :: RdrName -> Bool
doesNotNeedExtraParens = \case
  Exact name ->
    let s = nameStableString name
     in -- I'm not sure this "stable string" is stable enough, but it looks
        -- like this is the most robust way to tell if we're looking at
        -- exactly this piece of built-in syntax.
        ("$ghc-prim$GHC.Tuple$" `isPrefixOf` s)
          || ("$ghc-prim$GHC.Types$[]" `isPrefixOf` s)
  _ -> False

p_qualName :: ModuleName -> OccName -> R ()
p_qualName mname occName = do
  atom mname
  txt "."
  atom occName

-- | A helper for formatting infix constructions in lhs of definitions.
p_infixDefHelper ::
  -- | Whether to format in infix style
  Bool ->
  -- | Whether to bump indentation for arguments
  Bool ->
  -- | How to print the operator\/name
  R () ->
  -- | How to print the arguments
  [R ()] ->
  R ()
p_infixDefHelper isInfix indentArgs name args =
  case (isInfix, args) of
    (True, p0 : p1 : ps) -> do
      let parens' =
            if null ps
              then id
              else parens N
      parens' $ do
        p0
        breakpoint
        inci . sitcc $ do
          name
          space
          p1
      unless (null ps) . inciIf indentArgs $ do
        breakpoint
        sitcc (sep breakpoint sitcc ps)
    (_, ps) -> do
      name
      unless (null ps) $ do
        breakpoint
        inciIf indentArgs $ sitcc (sep breakpoint sitcc args)

-- | Print a Haddock.
p_hsDocString ::
  -- | Haddock style
  HaddockStyle ->
  -- | Finish the doc string with a newline
  Bool ->
  -- | The doc string to render
  LHsDocString ->
  R ()
p_hsDocString hstyle needsNewline (L l str) = do
  let isCommentSpan = \case
        HaddockSpan _ _ -> True
        CommentSpan _ -> True
        _ -> False
  goesAfterComment <- maybe False isCommentSpan <$> getSpanMark
  -- Make sure the Haddock is separated by a newline from other comments.
  when goesAfterComment newline
  forM_ (zip (splitDocString str) (True : repeat False)) $ \(x, isFirst) -> do
    if isFirst
      then case hstyle of
        Pipe -> txt "-- |"
        Caret -> txt "-- ^"
        Asterisk n -> txt ("-- " <> T.replicate n "*")
        Named name -> p_hsDocName name
      else newline >> txt "--"
    space
    unless (T.null x) (txt x)
  when needsNewline newline
  case l of
    UnhelpfulSpan _ ->
      -- It's often the case that the comment itself doesn't have a span
      -- attached to it and instead its location can be obtained from
      -- nearest enclosing span.
      getEnclosingSpan (const True) >>= mapM_ (setSpanMark . HaddockSpan hstyle)
    RealSrcSpan spn -> setSpanMark (HaddockSpan hstyle spn)

-- | Print anchor of named doc section.
p_hsDocName :: String -> R ()
p_hsDocName name = txt ("-- $" <> T.pack name)
