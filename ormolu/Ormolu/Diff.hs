{-# LANGUAGE RankNTypes #-}

-- | Diffing GHC ASTs modulo span positions.
module Ormolu.Diff
  ( Diff (..),
    diffParseResult,
    diffText,
  )
where

import Data.ByteString (ByteString)
import Data.Generics
import Data.Text (Text)
import qualified Data.Text as T
import qualified FastString as GHC
import GHC
import Ormolu.Imports (normalizeImports)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Utils

-- | Result of comparing two 'ParseResult's.
data Diff
  = -- | Two parse results are the same
    Same
  | -- | Two parse results differ
    Different [SrcSpan]

instance Semigroup Diff where
  Same <> a = a
  a <> Same = a
  Different xs <> Different ys = Different (xs ++ ys)

instance Monoid Diff where
  mempty = Same

-- | Return 'Diff' of two 'ParseResult's.
diffParseResult :: ParseResult -> ParseResult -> Diff
diffParseResult
  ParseResult
    { prCommentStream = cstream0,
      prParsedSource = hs0
    }
  ParseResult
    { prCommentStream = cstream1,
      prParsedSource = hs1
    } =
    matchIgnoringSrcSpans cstream0 cstream1
      <> matchIgnoringSrcSpans
        hs0 {hsmodImports = normalizeImports (hsmodImports hs0)}
        hs1 {hsmodImports = normalizeImports (hsmodImports hs1)}

-- | Compare two values for equality disregarding differences in 'SrcSpan's
-- and the ordering of import lists.
matchIgnoringSrcSpans :: Data a => a -> a -> Diff
matchIgnoringSrcSpans = genericQuery
  where
    genericQuery :: GenericQ (GenericQ Diff)
    genericQuery x y
      -- 'ByteString' implements 'Data' instance manually and does not
      -- implement 'toConstr', so we have to deal with it in a special way.
      | Just x' <- cast x,
        Just y' <- cast y =
        if x' == (y' :: ByteString)
          then Same
          else Different []
      | typeOf x == typeOf y,
        toConstr x == toConstr y =
        mconcat $
          gzipWithQ
            ( genericQuery
                `extQ` srcSpanEq
                `extQ` commentEq
                `extQ` sourceTextEq
                `extQ` hsDocStringEq
                `extQ` importDeclQualifiedStyleEq
                `ext2Q` forLocated
            )
            x
            y
      | otherwise = Different []
    srcSpanEq :: SrcSpan -> GenericQ Diff
    srcSpanEq _ _ = Same
    commentEq :: Comment -> GenericQ Diff
    commentEq (Comment _ x) d =
      case cast d :: Maybe Comment of
        Nothing -> Different []
        Just (Comment _ y) ->
          if x == y
            then Same
            else Different []
    sourceTextEq :: SourceText -> GenericQ Diff
    sourceTextEq _ _ = Same
    importDeclQualifiedStyleEq :: ImportDeclQualifiedStyle -> GenericQ Diff
    importDeclQualifiedStyleEq d0 d1' =
      case (d0, cast d1' :: Maybe ImportDeclQualifiedStyle) of
        (x, Just x') | x == x' -> Same
        (QualifiedPre, Just QualifiedPost) -> Same
        (QualifiedPost, Just QualifiedPre) -> Same
        _ -> Different []
    hsDocStringEq :: HsDocString -> GenericQ Diff
    hsDocStringEq str0 str1' =
      case cast str1' :: Maybe HsDocString of
        Nothing -> Different []
        Just str1 ->
          if splitDocString str0 == splitDocString str1
            then Same
            else Different []
    forLocated ::
      (Data e0, Data e1) =>
      GenLocated e0 e1 ->
      GenericQ Diff
    forLocated x@(L mspn _) y =
      maybe id appendSpan (cast mspn) (genericQuery x y)
    appendSpan :: SrcSpan -> Diff -> Diff
    appendSpan s (Different ss) | fresh && helpful = Different (s : ss)
      where
        fresh = not $ any (`isSubspanOf` s) ss
        helpful = isGoodSrcSpan s
    appendSpan _ d = d

-- | Diff two texts and return the location they start to differ, alongside
-- with excerpts around that location.
diffText ::
  -- | Text before
  Text ->
  -- | Text after
  Text ->
  -- | Path to use to construct 'GHC.RealSrcLoc'
  FilePath ->
  Maybe (GHC.RealSrcLoc, Text, Text)
diffText left right fp =
  case go (0, 0, 0) left right of
    Nothing -> Nothing
    Just (row, col, loc) ->
      Just
        ( GHC.mkRealSrcLoc (GHC.mkFastString fp) row col,
          getSpan loc left,
          getSpan loc right
        )
  where
    go (row, col, loc) t1 t2 =
      case (T.uncons t1, T.uncons t2) of
        -- both text empty, all good
        (Nothing, Nothing) ->
          Nothing
        -- first chars are the same, adjust position and recurse
        (Just (c1, r1), Just (c2, r2))
          | c1 == c2 ->
            let (row', col', loc') =
                  if c1 == '\n'
                    then (row + 1, 0, loc + 1)
                    else (row, col + 1, loc + 1)
             in go (row', col', loc') r1 r2
        -- something is different, return the position
        _ ->
          Just (row, col, loc)
    getSpan loc = T.take 20 . T.drop (loc - 10)
