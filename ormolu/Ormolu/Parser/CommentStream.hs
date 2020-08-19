{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Functions for working with comment stream.
module Ormolu.Parser.CommentStream
  ( -- * Comment stream
    CommentStream (..),
    mkCommentStream,
    showCommentStream,

    -- * Comment
    Comment (..),
    unComment,
    hasAtomsBefore,
    isMultilineComment,
  )
where

import Data.Char (isSpace)
import Data.Data (Data)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified GHC
import qualified Lexer as GHC
import Ormolu.Parser.Pragma
import Ormolu.Parser.Shebang
import Ormolu.Processing.Common
import Ormolu.Utils (onTheSameLine, showOutputable)
import SrcLoc

----------------------------------------------------------------------------
-- Comment stream

-- | A stream of 'RealLocated' 'Comment's in ascending order with respect to
-- beginning of corresponding spans.
newtype CommentStream = CommentStream [RealLocated Comment]
  deriving (Eq, Data, Semigroup, Monoid)

-- | Create 'CommentStream' from 'GHC.PState'. The pragmas and shebangs are
-- removed from the 'CommentStream'. Shebangs are only extracted from the
-- comments that come from the first argument.
mkCommentStream ::
  -- | Original input
  String ->
  -- | Extra comments to include
  [Located String] ->
  -- | Parser state to use for comment extraction
  GHC.PState ->
  -- | Stack header, shebangs, pragmas, and comment stream
  ( Maybe (RealLocated Comment),
    [Shebang],
    [([RealLocated Comment], Pragma)],
    CommentStream
  )
mkCommentStream input extraComments pstate =
  ( mstackHeader,
    shebangs,
    pragmas,
    CommentStream comments
  )
  where
    (comments, pragmas) = extractPragmas input rawComments1
    (rawComments1, mstackHeader) = extractStackHeader rawComments0
    rawComments0 =
      L.sortOn (realSrcSpanStart . getRealSrcSpan) . mapMaybe toRealSpan $
        otherExtraComments
          ++ mapMaybe (liftMaybe . fmap unAnnotationComment) (GHC.comment_q pstate)
          ++ concatMap
            (mapMaybe (liftMaybe . fmap unAnnotationComment) . snd)
            (GHC.annotations_comments pstate)
    (shebangs, otherExtraComments) = extractShebangs extraComments

-- | Pretty-print a 'CommentStream'.
showCommentStream :: CommentStream -> String
showCommentStream (CommentStream xs) =
  unlines $
    showComment <$> xs
  where
    showComment (GHC.L l str) = showOutputable l ++ " " ++ show str

----------------------------------------------------------------------------
-- Comment

-- | A wrapper for a single comment. The 'Bool' indicates whether there were
-- atoms before beginning of the comment in the original input. The
-- 'NonEmpty' list inside contains lines of multiline comment @{- … -}@ or
-- just single item\/line otherwise.
data Comment = Comment Bool (NonEmpty String)
  deriving (Eq, Show, Data)

-- | Normalize comment string. Sometimes one multi-line comment is turned
-- into several lines for subsequent outputting with correct indentation for
-- each line.
mkComment ::
  -- | Lines of original input with their indices
  [(Int, String)] ->
  -- | Raw comment string
  RealLocated String ->
  -- | Remaining lines of original input and the constructed 'Comment'
  ([(Int, String)], RealLocated Comment)
mkComment ls (L l s) = (ls', comment)
  where
    comment =
      L l . Comment atomsBefore . removeConseqBlanks . fmap dropTrailing $
        if "{-" `L.isPrefixOf` s
          then case NE.nonEmpty (lines s) of
            Nothing -> s :| []
            Just (x :| xs) ->
              let getIndent y =
                    if all isSpace y || y == endDisabling
                      then startIndent
                      else length (takeWhile isSpace y)
                  n = minimum (startIndent : fmap getIndent xs)
                  removeIndent y =
                    if y == endDisabling
                      then y
                      else drop n y
               in x :| (removeIndent <$> xs)
          else s :| []
    (atomsBefore, ls') =
      case dropWhile ((< commentLine) . fst) ls of
        [] -> (False, [])
        ((_, i) : ls'') ->
          case take 2 (dropWhile isSpace i) of
            "--" -> (False, ls'')
            "{-" -> (False, ls'')
            _ -> (True, ls'')
    dropTrailing = L.dropWhileEnd isSpace
    startIndent = srcSpanStartCol l - 1
    commentLine = srcSpanStartLine l

-- | Get a collection of lines from a 'Comment'.
unComment :: Comment -> NonEmpty String
unComment (Comment _ xs) = xs

-- | Check whether the 'Comment' had some non-whitespace atoms in front of
-- it in the original input.
hasAtomsBefore :: Comment -> Bool
hasAtomsBefore (Comment atomsBefore _) = atomsBefore

-- | Is this comment multiline-style?
isMultilineComment :: Comment -> Bool
isMultilineComment (Comment _ (x :| _)) = "{-" `L.isPrefixOf` x

----------------------------------------------------------------------------
-- Helpers

-- | Detect and extract stack header if it is present.
extractStackHeader ::
  -- | Comment stream to analyze
  [RealLocated String] ->
  ([RealLocated String], Maybe (RealLocated Comment))
extractStackHeader = \case
  [] -> ([], Nothing)
  (x : xs) ->
    let comment = snd (mkComment [] x)
     in if isStackHeader (unRealSrcSpan comment)
          then (xs, Just comment)
          else (x : xs, Nothing)
  where
    isStackHeader (Comment _ (x :| _)) =
      "stack" `L.isPrefixOf` dropWhile isSpace (drop 2 x)

-- | Extract pragmas and their associated comments.
extractPragmas ::
  -- | Input
  String ->
  -- | Comment stream to analyze
  [RealLocated String] ->
  ([RealLocated Comment], [([RealLocated Comment], Pragma)])
extractPragmas input = go initialLs id id
  where
    initialLs = zip [1 ..] (lines input)
    go ls csSoFar pragmasSoFar = \case
      [] -> (csSoFar [], pragmasSoFar [])
      (x : xs) ->
        case parsePragma (unRealSrcSpan x) of
          Nothing ->
            let (ls', x') = mkComment ls x
             in go ls' (csSoFar . (x' :)) pragmasSoFar xs
          Just pragma ->
            let combined ys = (csSoFar ys, pragma)
                go' ls' ys rest = go ls' id (pragmasSoFar . (combined ys :)) rest
             in case xs of
                  [] -> go' ls [] xs
                  (y : ys) ->
                    let (ls', y') = mkComment ls y
                     in if onTheSameLine
                          (RealSrcSpan (getRealSrcSpan x))
                          (RealSrcSpan (getRealSrcSpan y))
                          then go' ls' [y'] ys
                          else go' ls [] xs

-- | Get a 'String' from 'GHC.AnnotationComment'.
unAnnotationComment :: GHC.AnnotationComment -> Maybe String
unAnnotationComment = \case
  GHC.AnnDocCommentNext _ -> Nothing -- @-- |@
  GHC.AnnDocCommentPrev _ -> Nothing -- @-- ^@
  GHC.AnnDocCommentNamed _ -> Nothing -- @-- $@
  GHC.AnnDocSection _ _ -> Nothing -- @-- *@
  GHC.AnnDocOptions s -> Just s
  GHC.AnnLineComment s -> Just s
  GHC.AnnBlockComment s -> Just s

liftMaybe :: Located (Maybe a) -> Maybe (Located a)
liftMaybe = \case
  L _ Nothing -> Nothing
  L l (Just a) -> Just (L l a)

toRealSpan :: Located a -> Maybe (RealLocated a)
toRealSpan (L (RealSrcSpan l) a) = Just (L l a)
toRealSpan _ = Nothing

-- | Remove consecutive blank lines.
removeConseqBlanks :: NonEmpty String -> NonEmpty String
removeConseqBlanks (x :| xs) = x :| go (null x) id xs
  where
    go seenBlank acc = \case
      [] -> acc []
      (y : ys) ->
        if seenBlank && null y
          then go True acc ys
          else go (null y) (acc . (y :)) ys
