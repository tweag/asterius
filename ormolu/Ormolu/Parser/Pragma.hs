{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module for parsing of pragmas from comments.
module Ormolu.Parser.Pragma
  ( Pragma (..),
    parsePragma,
  )
where

import Control.Monad
import Data.Char (isSpace, toLower)
import qualified Data.List as L
import qualified EnumSet as ES
import FastString (mkFastString, unpackFS)
import qualified Lexer as L
import Module (ComponentId (..), newSimpleUnitId)
import SrcLoc
import StringBuffer

-- | Ormolu's representation of pragmas.
data Pragma
  = -- | Language pragma
    PragmaLanguage [String]
  | -- | GHC options pragma
    PragmaOptionsGHC String
  | -- | Haddock options pragma
    PragmaOptionsHaddock String
  deriving (Show, Eq)

-- | Extract a pragma from a comment if possible, or return 'Nothing'
-- otherwise.
parsePragma ::
  -- | Comment to try to parse
  String ->
  Maybe Pragma
parsePragma input = do
  inputNoPrefix <- L.stripPrefix "{-#" input
  guard ("#-}" `L.isSuffixOf` input)
  let contents = take (length inputNoPrefix - 3) inputNoPrefix
      (pragmaName, cs) = (break isSpace . dropWhile isSpace) contents
  case toLower <$> pragmaName of
    "language" -> PragmaLanguage <$> parseExtensions cs
    "options_ghc" -> Just $ PragmaOptionsGHC (trimSpaces cs)
    "options_haddock" -> Just $ PragmaOptionsHaddock (trimSpaces cs)
    _ -> Nothing
  where
    trimSpaces :: String -> String
    trimSpaces = L.dropWhileEnd isSpace . dropWhile isSpace

-- | Assuming the input consists of a series of tokens from a language
-- pragma, return the set of enabled extensions.
parseExtensions :: String -> Maybe [String]
parseExtensions str = tokenize str >>= go
  where
    go = \case
      [L.ITconid ext] -> return [unpackFS ext]
      (L.ITconid ext : L.ITcomma : xs) -> (unpackFS ext :) <$> go xs
      _ -> Nothing

-- | Tokenize a given input using GHC's lexer.
tokenize :: String -> Maybe [L.Token]
tokenize input =
  case L.unP pLexer parseState of
    L.PFailed {} -> Nothing
    L.POk _ x -> Just x
  where
    location = mkRealSrcLoc (mkFastString "") 1 1
    buffer = stringToStringBuffer input
    parseState = L.mkPStatePure parserFlags buffer location
    parserFlags =
      L.mkParserFlags'
        ES.empty
        ES.empty
        (newSimpleUnitId (ComponentId (mkFastString "")))
        True
        True
        True
        True

-- | Haskell lexer.
pLexer :: L.P [L.Token]
pLexer = go
  where
    go = do
      r <- L.lexer False return
      case unLoc r of
        L.ITeof -> return []
        x -> (x :) <$> go
