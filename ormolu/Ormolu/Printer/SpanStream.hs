{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Build span stream from AST.
module Ormolu.Printer.SpanStream
  ( SpanStream (..),
    mkSpanStream,
  )
where

import Data.DList (DList)
import qualified Data.DList as D
import Data.Data (Data)
import Data.Generics (everything, ext2Q)
import Data.List (sortOn)
import Data.Typeable (cast)
import SrcLoc

-- | A stream of 'RealSrcSpan's in ascending order. This allows us to tell
-- e.g. whether there is another \"located\" element of AST between current
-- element and comment we're considering for printing.
newtype SpanStream = SpanStream [RealSrcSpan]
  deriving (Eq, Show, Data, Semigroup, Monoid)

-- | Create 'SpanStream' from a data structure containing \"located\"
-- elements.
mkSpanStream ::
  Data a =>
  -- | Data structure to inspect (AST)
  a ->
  SpanStream
mkSpanStream a =
  SpanStream
    . sortOn realSrcSpanStart
    . D.toList
    $ everything mappend (const mempty `ext2Q` queryLocated) a
  where
    queryLocated ::
      (Data e0) =>
      GenLocated e0 e1 ->
      DList RealSrcSpan
    queryLocated (L mspn _) =
      case cast mspn :: Maybe SrcSpan of
        Nothing -> mempty
        Just (UnhelpfulSpan _) -> mempty
        Just (RealSrcSpan spn) -> D.singleton spn
