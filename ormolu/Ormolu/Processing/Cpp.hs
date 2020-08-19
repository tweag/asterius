{-# LANGUAGE OverloadedStrings #-}

-- | Support for CPP.
module Ormolu.Processing.Cpp
  ( State (..),
    processLine,
    unmaskLine,
  )
where

import Control.Monad
import Data.Char (isSpace)
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

-- | State of the CPP processor.
data State
  = -- | Outside of CPP directives
    Outside
  | -- | In a conditional expression
    InConditional
  | -- | In a continuation (after @\\@)
    InContinuation
  deriving (Eq, Show)

-- | Automatically mask the line when needed and update the 'State'.
processLine :: String -> State -> (String, State)
processLine line state
  | for "define " = (masked, state')
  | for "include " = (masked, state')
  | for "undef " = (masked, state')
  | for "ifdef " = (masked, InConditional)
  | for "ifndef " = (masked, InConditional)
  | for "if " = (masked, InConditional)
  | for "else" = (masked, InConditional)
  | for "elif" = (masked, InConditional)
  | for "endif" = (masked, state')
  | otherwise =
    case state of
      Outside -> (line, Outside)
      InConditional -> (masked, InConditional)
      InContinuation -> (masked, state')
  where
    for directive = isJust $ do
      s <- dropWhile isSpace <$> L.stripPrefix "#" line
      void (L.stripPrefix directive s)
    masked = maskLine line
    state' =
      if "\\" `L.isSuffixOf` line
        then InContinuation
        else Outside

-- | Mask the given line.
maskLine :: String -> String
maskLine x = maskPrefix ++ x

-- | If the given line is masked, unmask it. Otherwise return the line
-- unchanged.
unmaskLine :: Text -> Text
unmaskLine x =
  case T.stripPrefix maskPrefix (T.stripStart x) of
    Nothing -> x
    Just x' -> x'

-- | Mask prefix for CPP.
maskPrefix :: IsString s => s
maskPrefix = "-- ORMOLU_CPP_MASK"
