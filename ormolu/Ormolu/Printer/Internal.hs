{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | In most cases import "Ormolu.Printer.Combinators" instead, these
-- functions are the low-level building blocks and should not be used on
-- their own. The 'R' monad is re-exported from "Ormolu.Printer.Combinators"
-- as well.
module Ormolu.Printer.Internal
  ( -- * The 'R' monad
    R,
    runR,

    -- * Internal functions
    txt,
    interferingTxt,
    atom,
    space,
    newline,
    useRecordDot,
    inci,
    sitcc,
    Layout (..),
    enterLayout,
    vlayout,
    getLayout,

    -- * Helpers for braces
    useBraces,
    dontUseBraces,
    canUseBraces,

    -- * Special helpers for comment placement
    CommentPosition (..),
    registerPendingCommentLine,
    trimSpanStream,
    nextEltSpan,
    popComment,
    getEnclosingSpan,
    withEnclosingSpan,
    thisLineSpans,

    -- * Stateful markers
    SpanMark (..),
    spanMarkSpan,
    HaddockStyle (..),
    setSpanMark,
    getSpanMark,

    -- * Annotations
    getAnns,
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bool (bool)
import Data.Coerce
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder
import GHC
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream
import Ormolu.Printer.SpanStream
import Ormolu.Utils (showOutputable)
import Outputable (Outputable)

----------------------------------------------------------------------------
-- The 'R' monad

-- | The 'R' monad hosts combinators that allow us to describe how to render
-- AST.
newtype R a = R (ReaderT RC (State SC) a)
  deriving (Functor, Applicative, Monad)

-- | Reader context of 'R'. This should be used when we control rendering by
-- enclosing certain expressions with wrappers.
data RC = RC
  { -- | Indentation level, as the column index we need to start from after
    -- a newline if we break lines
    rcIndent :: !Int,
    -- | Current layout
    rcLayout :: Layout,
    -- | Spans of enclosing elements of AST
    rcEnclosingSpans :: [RealSrcSpan],
    -- | Collection of annotations
    rcAnns :: Anns,
    -- | Whether the last expression in the layout can use braces
    rcCanUseBraces :: Bool,
    -- | Whether the source could have used the record dot preprocessor
    rcUseRecDot :: Bool
  }

-- | State context of 'R'.
data SC = SC
  { -- | Index of the next column to render
    scColumn :: !Int,
    -- | Indentation level that was used for the current line
    scIndent :: !Int,
    -- | Rendered source code so far
    scBuilder :: Builder,
    -- | Span stream
    scSpanStream :: SpanStream,
    -- | Spans of atoms that have been printed on the current line so far
    scThisLineSpans :: [RealSrcSpan],
    -- | Comment stream
    scCommentStream :: CommentStream,
    -- | Pending comment lines (in reverse order) to be inserted before next
    -- newline, 'Int' is the indentation level
    scPendingComments :: ![(CommentPosition, Text)],
    -- | Whether to output a space before the next output
    scRequestedDelimiter :: !RequestedDelimiter,
    -- | An auxiliary marker for keeping track of last output element
    scSpanMark :: !(Maybe SpanMark)
  }

-- | Make sure next output is delimited by one of the following.
data RequestedDelimiter
  = -- | A space
    RequestedSpace
  | -- | A newline
    RequestedNewline
  | -- | Nothing
    RequestedNothing
  | -- | We just output a newline
    AfterNewline
  | -- | We haven't printed anything yet
    VeryBeginning
  deriving (Eq, Show)

-- | 'Layout' options.
data Layout
  = -- | Put everything on single line
    SingleLine
  | -- | Use multiple lines
    MultiLine
  deriving (Eq, Show)

-- | Modes for rendering of pending comments.
data CommentPosition
  = -- | Put the comment on the same line
    OnTheSameLine
  | -- | Put the comment on next line
    OnNextLine
  deriving (Eq, Show)

-- | Run an 'R' monad.
runR ::
  -- | Monad to run
  R () ->
  -- | Span stream
  SpanStream ->
  -- | Comment stream
  CommentStream ->
  -- | Annotations
  Anns ->
  -- | Use Record Dot Syntax
  Bool ->
  -- | Resulting rendition
  Text
runR (R m) sstream cstream anns recDot =
  TL.toStrict . toLazyText . scBuilder $ execState (runReaderT m rc) sc
  where
    rc =
      RC
        { rcIndent = 0,
          rcLayout = MultiLine,
          rcEnclosingSpans = [],
          rcAnns = anns,
          rcCanUseBraces = False,
          rcUseRecDot = recDot
        }
    sc =
      SC
        { scColumn = 0,
          scIndent = 0,
          scBuilder = mempty,
          scSpanStream = sstream,
          scThisLineSpans = [],
          scCommentStream = cstream,
          scPendingComments = [],
          scRequestedDelimiter = VeryBeginning,
          scSpanMark = Nothing
        }

----------------------------------------------------------------------------
-- Internal functions

-- | Type of the thing to output. Influences the primary low-level rendering
-- function 'spit'.
data SpitType
  = -- | Simple opaque text that breaks comment series.
    SimpleText
  | -- | Like 'SimpleText', but assume that when this text is inserted it
    -- will separate an 'Atom' and its pending comments, so insert an extra
    -- 'newline' in that case to force the pending comments and continue on
    -- a fresh line.
    InterferingText
  | -- | An atom that typically have span information in the AST and can
    -- have comments attached to it.
    Atom
  | -- | Used for rendering comment lines.
    CommentPart
  deriving (Show, Eq)

-- | Output a fixed 'Text' fragment. The argument may not contain any line
-- breaks. 'txt' is used to output all sorts of “fixed” bits of syntax like
-- keywords and pipes @|@ in functional dependencies.
--
-- To separate various bits of syntax with white space use 'space' instead
-- of @'txt' " "@. To output 'Outputable' Haskell entities like numbers use
-- 'atom'.
txt ::
  -- | 'Text' to output
  Text ->
  R ()
txt = spit SimpleText

-- | Similar to 'txt' but the text inserted this way is assumed to break the
-- “link” between the preceding atom and its pending comments.
interferingTxt ::
  -- | 'Text' to output
  Text ->
  R ()
interferingTxt = spit InterferingText

-- | Output 'Outputable' fragment of AST. This can be used to output numeric
-- literals and similar. Everything that doesn't have inner structure but
-- does have an 'Outputable' instance.
atom ::
  Outputable a =>
  a ->
  R ()
atom = spit Atom . T.pack . showOutputable

-- | Low-level non-public helper to define 'txt' and 'atom'.
spit ::
  -- | Type of the thing to spit
  SpitType ->
  -- | 'Text' to output
  Text ->
  R ()
spit _ "" = return ()
spit stype text = do
  requestedDel <- R (gets scRequestedDelimiter)
  pendingComments <- R (gets scPendingComments)
  when (stype == InterferingText && not (null pendingComments)) newline
  case requestedDel of
    RequestedNewline -> do
      R . modify $ \sc ->
        sc
          { scRequestedDelimiter = RequestedNothing
          }
      case stype of
        CommentPart -> newlineRaw
        _ -> newline
    _ -> return ()
  R $ do
    i <- asks rcIndent
    c <- gets scColumn
    closestEnclosing <- listToMaybe <$> asks rcEnclosingSpans
    let indentedTxt = spaces <> text
        spaces = T.replicate spacesN " "
        spacesN =
          if c == 0
            then i
            else bool 0 1 (requestedDel == RequestedSpace)
    modify $ \sc ->
      sc
        { scBuilder = scBuilder sc <> fromText indentedTxt,
          scColumn = scColumn sc + T.length indentedTxt,
          scIndent =
            if c == 0
              then i
              else scIndent sc,
          scThisLineSpans =
            let xs = scThisLineSpans sc
             in case stype of
                  Atom -> case closestEnclosing of
                    Nothing -> xs
                    Just x -> x : xs
                  _ -> xs,
          scRequestedDelimiter = RequestedNothing,
          scSpanMark =
            -- If there are pending comments, do not reset last comment
            -- location.
            if (stype == CommentPart) || (not . null . scPendingComments) sc
              then scSpanMark sc
              else Nothing
        }

-- | This primitive /does not/ necessarily output a space. It just ensures
-- that the next thing that will be printed on the same line will be
-- separated by a single space from the previous output. Using this
-- combinator twice results in at most one space.
--
-- In practice this design prevents trailing white space and makes it hard
-- to output more than one delimiting space in a row, which is what we
-- usually want.
space :: R ()
space = R . modify $ \sc ->
  sc
    { scRequestedDelimiter = case scRequestedDelimiter sc of
        RequestedNothing -> RequestedSpace
        other -> other
    }

-- | Output a newline. First time 'newline' is used after some non-'newline'
-- output it gets inserted immediately. Second use of 'newline' does not
-- output anything but makes sure that the next non-white space output will
-- be prefixed by a newline. Using 'newline' more than twice in a row has no
-- effect. Also, using 'newline' at the very beginning has no effect, this
-- is to avoid leading whitespace.
--
-- Similarly to 'space', this design prevents trailing newlines and makes it
-- hard to output more than one blank newline in a row.
newline :: R ()
newline = do
  indent <- R (gets scIndent)
  cs <- reverse <$> R (gets scPendingComments)
  case cs of
    [] -> newlineRaw
    ((position, _) : _) -> do
      case position of
        OnTheSameLine -> space
        OnNextLine -> newlineRaw
      R . forM_ cs $ \(_, text) ->
        let modRC rc =
              rc
                { rcIndent = indent
                }
            R m = do
              unless (T.null text) $
                spit CommentPart text
              newlineRaw
         in local modRC m
      R . modify $ \sc ->
        sc
          { scPendingComments = []
          }

-- | Low-level newline primitive. This one always just inserts a newline, no
-- hooks can be attached.
newlineRaw :: R ()
newlineRaw = R . modify $ \sc ->
  let requestedDel = scRequestedDelimiter sc
      builderSoFar = scBuilder sc
   in sc
        { scBuilder = case requestedDel of
            AfterNewline -> builderSoFar
            RequestedNewline -> builderSoFar
            VeryBeginning -> builderSoFar
            _ -> builderSoFar <> "\n",
          scColumn = 0,
          scIndent = 0,
          scThisLineSpans = [],
          scRequestedDelimiter = case scRequestedDelimiter sc of
            AfterNewline -> RequestedNewline
            RequestedNewline -> RequestedNewline
            VeryBeginning -> VeryBeginning
            _ -> AfterNewline
        }

-- | Return 'True' if we should print record dot syntax.
useRecordDot :: R Bool
useRecordDot = R (asks rcUseRecDot)

-- | Increase indentation level by one indentation step for the inner
-- computation. 'inci' should be used when a part of code must be more
-- indented relative to the parts outside of 'inci' in order for the output
-- to be valid Haskell. When layout is single-line there is no obvious
-- effect, but with multi-line layout correct indentation levels matter.
inci :: R () -> R ()
inci (R m) = R (local modRC m)
  where
    modRC rc =
      rc
        { rcIndent = rcIndent rc + indentStep
        }

-- | Set indentation level for the inner computation equal to current
-- column. This makes sure that the entire inner block is uniformly
-- \"shifted\" to the right.
sitcc :: R () -> R ()
sitcc (R m) = do
  requestedDel <- R (gets scRequestedDelimiter)
  i <- R (asks rcIndent)
  c <- R (gets scColumn)
  let modRC rc =
        rc
          { rcIndent = max i (c + bool 0 1 (requestedDel == RequestedSpace))
          }
  R (local modRC m)

-- | Set 'Layout' for internal computation.
enterLayout :: Layout -> R () -> R ()
enterLayout l (R m) = R (local modRC m)
  where
    modRC rc =
      rc
        { rcLayout = l
        }

-- | Do one or another thing depending on current 'Layout'.
vlayout ::
  -- | Single line
  R a ->
  -- | Multi line
  R a ->
  R a
vlayout sline mline = do
  l <- getLayout
  case l of
    SingleLine -> sline
    MultiLine -> mline

-- | Get current 'Layout'.
getLayout :: R Layout
getLayout = R (asks rcLayout)

----------------------------------------------------------------------------
-- Special helpers for comment placement

-- | Register a comment line for outputting. It will be inserted right
-- before next newline. When the comment goes after something else on the
-- same line, a space will be inserted between preceding text and the
-- comment when necessary.
registerPendingCommentLine ::
  -- | Comment position
  CommentPosition ->
  -- | 'Text' to output
  Text ->
  R ()
registerPendingCommentLine position text = R $ do
  modify $ \sc ->
    sc
      { scPendingComments = (position, text) : scPendingComments sc
      }

-- | Drop elements that begin before or at the same place as given
-- 'SrcSpan'.
trimSpanStream ::
  -- | Reference span
  RealSrcSpan ->
  R ()
trimSpanStream ref = do
  let leRef :: RealSrcSpan -> Bool
      leRef x = realSrcSpanStart x <= realSrcSpanStart ref
  R . modify $ \sc ->
    sc
      { scSpanStream = coerce (dropWhile leRef) (scSpanStream sc)
      }

-- | Get location of next element in AST.
nextEltSpan :: R (Maybe RealSrcSpan)
nextEltSpan = listToMaybe . coerce <$> R (gets scSpanStream)

-- | Pop a 'Comment' from the 'CommentStream' if given predicate is
-- satisfied and there are comments in the stream.
popComment ::
  (RealLocated Comment -> Bool) ->
  R (Maybe (RealLocated Comment))
popComment f = R $ do
  CommentStream cstream <- gets scCommentStream
  case cstream of
    [] -> return Nothing
    (x : xs) ->
      if f x
        then
          Just x
            <$ modify
              ( \sc ->
                  sc
                    { scCommentStream = CommentStream xs
                    }
              )
        else return Nothing

-- | Get the first enclosing 'RealSrcSpan' that satisfies given predicate.
getEnclosingSpan ::
  -- | Predicate to use
  (RealSrcSpan -> Bool) ->
  R (Maybe RealSrcSpan)
getEnclosingSpan f =
  listToMaybe . filter f <$> R (asks rcEnclosingSpans)

-- | Set 'RealSrcSpan' of enclosing span for the given computation.
withEnclosingSpan :: RealSrcSpan -> R () -> R ()
withEnclosingSpan spn (R m) = R (local modRC m)
  where
    modRC rc =
      rc
        { rcEnclosingSpans = spn : rcEnclosingSpans rc
        }

-- | Get spans on this line so far.
thisLineSpans :: R [RealSrcSpan]
thisLineSpans = R (gets scThisLineSpans)

----------------------------------------------------------------------------
-- Stateful markers

-- | An auxiliary marker for keeping track of last output element.
data SpanMark
  = -- | Haddock comment
    HaddockSpan HaddockStyle RealSrcSpan
  | -- | Non-haddock comment
    CommentSpan RealSrcSpan
  | -- | A statement in a do-block and such span
    StatementSpan RealSrcSpan

-- | Project 'RealSrcSpan' from 'SpanMark'.
spanMarkSpan :: SpanMark -> RealSrcSpan
spanMarkSpan = \case
  HaddockSpan _ s -> s
  CommentSpan s -> s
  StatementSpan s -> s

-- | Haddock string style.
data HaddockStyle
  = -- | @-- |@
    Pipe
  | -- | @-- ^@
    Caret
  | -- | @-- *@
    Asterisk Int
  | -- | @-- $@
    Named String

-- | Set span of last output comment.
setSpanMark ::
  -- | Span mark to set
  SpanMark ->
  R ()
setSpanMark spnMark = R . modify $ \sc ->
  sc
    { scSpanMark = Just spnMark
    }

-- | Get span of last output comment.
getSpanMark :: R (Maybe SpanMark)
getSpanMark = R (gets scSpanMark)

----------------------------------------------------------------------------
-- Annotations

-- | For a given span return 'AnnKeywordId's associated with it.
getAnns ::
  SrcSpan ->
  R [AnnKeywordId]
getAnns spn = lookupAnns spn <$> R (asks rcAnns)

----------------------------------------------------------------------------
-- Helpers for braces

-- | Make the inner computation use braces around single-line layouts.
useBraces :: R () -> R ()
useBraces (R r) = R (local (\i -> i {rcCanUseBraces = True}) r)

-- | Make the inner computation omit braces around single-line layouts.
dontUseBraces :: R () -> R ()
dontUseBraces (R r) = R (local (\i -> i {rcCanUseBraces = False}) r)

-- | Return 'True' if we can use braces in this context.
canUseBraces :: R Bool
canUseBraces = R (asks rcCanUseBraces)

----------------------------------------------------------------------------
-- Constants

-- | Indentation step.
indentStep :: Int
indentStep = 2
