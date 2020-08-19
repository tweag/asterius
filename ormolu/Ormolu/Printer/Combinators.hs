{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Printing combinators. The definitions here are presented in such an
-- order so you can just go through the Haddocks and by the end of the file
-- you should have a pretty good idea how to program rendering logic.
module Ormolu.Printer.Combinators
  ( -- * The 'R' monad
    R,
    runR,
    getAnns,
    getEnclosingSpan,

    -- * Combinators

    -- ** Basic
    txt,
    atom,
    space,
    newline,
    inci,
    inciIf,
    located,
    located',
    switchLayout,
    Layout (..),
    vlayout,
    getLayout,
    breakpoint,
    breakpoint',

    -- ** Formatting lists
    sep,
    sepSemi,
    canUseBraces,
    useBraces,
    dontUseBraces,

    -- ** Wrapping
    BracketStyle (..),
    sitcc,
    backticks,
    banana,
    braces,
    brackets,
    parens,
    parensHash,
    pragmaBraces,
    pragma,

    -- ** Literals
    comma,
    commaDel,
    equals,

    -- ** Stateful markers
    SpanMark (..),
    spanMarkSpan,
    HaddockStyle (..),
    setSpanMark,
    getSpanMark,
  )
where

import Control.Monad
import Data.List (intersperse)
import Data.Text (Text)
import Ormolu.Printer.Comments
import Ormolu.Printer.Internal
import SrcLoc

----------------------------------------------------------------------------
-- Basic

-- | Indent the inner expression if the first argument is 'True'.
inciIf ::
  -- | Whether to indent
  Bool ->
  -- | The expression to indent
  R () ->
  R ()
inciIf b m = if b then inci m else m

-- | Enter a 'Located' entity. This combinator handles outputting comments
-- and sets layout (single-line vs multi-line) for the inner computation.
-- Roughly, the rule for using 'located' is that every time there is a
-- 'Located' wrapper, it should be “discharged” with a corresponding
-- 'located' invocation.
located ::
  -- | Thing to enter
  Located a ->
  -- | How to render inner value
  (a -> R ()) ->
  R ()
located (L (UnhelpfulSpan _) a) f = f a
located (L (RealSrcSpan l) a) f = do
  spitPrecedingComments l
  withEnclosingSpan l $
    switchLayout [RealSrcSpan l] (f a)
  spitFollowingComments l

-- | A version of 'located' with arguments flipped.
located' ::
  -- | How to render inner value
  (a -> R ()) ->
  -- | Thing to enter
  Located a ->
  R ()
located' = flip located

-- | Set layout according to combination of given 'SrcSpan's for a given.
-- Use this only when you need to set layout based on e.g. combined span of
-- several elements when there is no corresponding 'Located' wrapper
-- provided by GHC AST. It is relatively rare that this one is needed.
--
-- Given empty list this function will set layout to single line.
switchLayout ::
  -- | Span that controls layout
  [SrcSpan] ->
  -- | Computation to run with changed layout
  R () ->
  R ()
switchLayout spans' = enterLayout (spansLayout spans')

-- | Which layout combined spans result in?
spansLayout :: [SrcSpan] -> Layout
spansLayout = \case
  [] -> SingleLine
  (x : xs) ->
    if isOneLineSpan (foldr combineSrcSpans x xs)
      then SingleLine
      else MultiLine

-- | Insert a space if enclosing layout is single-line, or newline if it's
-- multiline.
--
-- > breakpoint = vlayout space newline
breakpoint :: R ()
breakpoint = vlayout space newline

-- | Similar to 'breakpoint' but outputs nothing in case of single-line
-- layout.
--
-- > breakpoint' = vlayout (return ()) newline
breakpoint' :: R ()
breakpoint' = vlayout (return ()) newline

----------------------------------------------------------------------------
-- Formatting lists

-- | Render a collection of elements inserting a separator between them.
sep ::
  -- | Separator
  R () ->
  -- | How to render an element
  (a -> R ()) ->
  -- | Elements to render
  [a] ->
  R ()
sep s f xs = sequence_ (intersperse s (f <$> xs))

-- | Render a collection of elements layout-sensitively using given printer,
-- inserting semicolons if necessary and respecting 'useBraces' and
-- 'dontUseBraces' combinators.
--
-- > useBraces $ sepSemi txt ["foo", "bar"]
-- >   == vlayout (txt "{ foo; bar }") (txt "foo\nbar")
--
-- > dontUseBraces $ sepSemi txt ["foo", "bar"]
-- >   == vlayout (txt "foo; bar") (txt "foo\nbar")
sepSemi ::
  -- | How to render an element
  (a -> R ()) ->
  -- | Elements to render
  [a] ->
  R ()
sepSemi f xs = vlayout singleLine multiLine
  where
    singleLine = do
      ub <- canUseBraces
      case xs of
        [] -> when ub $ txt "{}"
        xs' ->
          if ub
            then do
              txt "{"
              space
              sep (txt ";" >> space) (dontUseBraces . f) xs'
              space
              txt "}"
            else sep (txt ";" >> space) f xs'
    multiLine =
      sep newline (dontUseBraces . f) xs

----------------------------------------------------------------------------
-- Wrapping

-- | 'BracketStyle' controlling how closing bracket is rendered.
data BracketStyle
  = -- | Normal
    N
  | -- | Shifted one level
    S
  deriving (Eq, Show)

-- | Surround given entity by backticks.
backticks :: R () -> R ()
backticks m = do
  txt "`"
  m
  txt "`"

-- | Surround given entity by banana brackets (i.e., from arrow notation.)
banana :: R () -> R ()
banana = brackets_ True "(|" "|)" N

-- | Surround given entity by curly braces @{@ and  @}@.
braces :: BracketStyle -> R () -> R ()
braces = brackets_ False "{" "}"

-- | Surround given entity by square brackets @[@ and @]@.
brackets :: BracketStyle -> R () -> R ()
brackets = brackets_ False "[" "]"

-- | Surround given entity by parentheses @(@ and @)@.
parens :: BracketStyle -> R () -> R ()
parens = brackets_ False "(" ")"

-- | Surround given entity by @(# @ and @ #)@.
parensHash :: BracketStyle -> R () -> R ()
parensHash = brackets_ True "(#" "#)"

-- | Braces as used for pragmas: @{-#@ and @#-}@.
pragmaBraces :: R () -> R ()
pragmaBraces m = sitcc $ do
  txt "{-#"
  space
  m
  breakpoint
  inci (txt "#-}")

-- | Surround the body with a pragma name and 'pragmaBraces'.
pragma ::
  -- | Pragma text
  Text ->
  -- | Pragma body
  R () ->
  R ()
pragma pragmaText body = pragmaBraces $ do
  txt pragmaText
  breakpoint
  body

-- | A helper for defining wrappers like 'parens' and 'braces'.
brackets_ ::
  -- | Insert breakpoints around brackets
  Bool ->
  -- | Opening bracket
  Text ->
  -- | Closing bracket
  Text ->
  -- | Bracket style
  BracketStyle ->
  -- | Inner expression
  R () ->
  R ()
brackets_ needBreaks open close style m = sitcc (vlayout singleLine multiLine)
  where
    singleLine = do
      txt open
      when needBreaks space
      m
      when needBreaks space
      txt close
    multiLine = do
      txt open
      if needBreaks
        then newline >> inci m
        else space >> sitcc m
      newline
      inciIf (style == S) (txt close)

----------------------------------------------------------------------------
-- Literals

-- | Print @,@.
comma :: R ()
comma = txt ","

-- | Delimiting combination with 'comma'. To be used with 'sep'.
commaDel :: R ()
commaDel = comma >> breakpoint

-- | Print @=@. Do not use @'txt' "="@.
equals :: R ()
equals = interferingTxt "="
