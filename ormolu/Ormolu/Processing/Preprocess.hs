{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Preprocessing for input source code.
module Ormolu.Processing.Preprocess
  ( preprocess,
  )
where

import Control.Monad
import Data.Char (isSpace)
import qualified Data.List as L
import Data.Maybe (isJust, maybeToList)
import FastString
import Ormolu.Config (RegionDeltas (..))
import Ormolu.Parser.Shebang (isShebang)
import Ormolu.Processing.Common
import qualified Ormolu.Processing.Cpp as Cpp
import SrcLoc

-- | Transform given input possibly returning comments extracted from it.
-- This handles LINE pragmas, CPP, shebangs, and the magic comments for
-- enabling\/disabling of Ormolu.
preprocess ::
  -- | File name, just to use in the spans
  FilePath ->
  -- | Input to process
  String ->
  -- | Region deltas
  RegionDeltas ->
  -- | Literal prefix, pre-processed input, literal suffix, extra comments
  (String, String, String, [Located String])
preprocess path input RegionDeltas {..} =
  go 1 OrmoluEnabled Cpp.Outside id id regionLines
  where
    (prefixLines, otherLines) = splitAt regionPrefixLength (lines input)
    (regionLines, suffixLines) =
      let regionLength = length otherLines - regionSuffixLength
       in splitAt regionLength otherLines
    go !n ormoluState cppState inputSoFar csSoFar = \case
      [] ->
        let input' = unlines (inputSoFar [])
         in ( unlines prefixLines,
              case ormoluState of
                OrmoluEnabled -> input'
                OrmoluDisabled -> input' ++ endDisabling,
              unlines suffixLines,
              csSoFar []
            )
      (x : xs) ->
        let (x', ormoluState', cppState', cs) =
              processLine path n ormoluState cppState x
         in go
              (n + 1)
              ormoluState'
              cppState'
              (inputSoFar . (x' :))
              (csSoFar . (maybeToList cs ++))
              xs

-- | Transform a given line possibly returning a comment extracted from it.
processLine ::
  -- | File name, just to use in the spans
  FilePath ->
  -- | Line number of this line
  Int ->
  -- | Whether Ormolu is currently enabled
  OrmoluState ->
  -- | CPP state
  Cpp.State ->
  -- | The actual line
  String ->
  -- | Adjusted line and possibly a comment extracted from it
  (String, OrmoluState, Cpp.State, Maybe (Located String))
processLine path n ormoluState Cpp.Outside line
  | "{-# LINE" `L.isPrefixOf` line =
    let (pragma, res) = getPragma line
        size = length pragma
        ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (size + 1))
     in (res, ormoluState, Cpp.Outside, Just (L ss pragma))
  | isOrmoluEnable line =
    case ormoluState of
      OrmoluEnabled ->
        (enableMarker, OrmoluEnabled, Cpp.Outside, Nothing)
      OrmoluDisabled ->
        (endDisabling ++ enableMarker, OrmoluEnabled, Cpp.Outside, Nothing)
  | isOrmoluDisable line =
    case ormoluState of
      OrmoluEnabled ->
        (disableMarker ++ startDisabling, OrmoluDisabled, Cpp.Outside, Nothing)
      OrmoluDisabled ->
        (disableMarker, OrmoluDisabled, Cpp.Outside, Nothing)
  | isShebang line =
    let ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (length line))
     in ("", ormoluState, Cpp.Outside, Just (L ss line))
  | otherwise =
    let (line', cppState') = Cpp.processLine line Cpp.Outside
     in (line', ormoluState, cppState', Nothing)
  where
    mkSrcLoc' = mkSrcLoc (mkFastString path) n
processLine _ _ ormoluState cppState line =
  let (line', cppState') = Cpp.processLine line cppState
   in (line', ormoluState, cppState', Nothing)

-- | Take a line pragma and output its replacement (where line pragma is
-- replaced with spaces) and the contents of the pragma itself.
getPragma ::
  -- | Pragma line to analyze
  String ->
  -- | Contents of the pragma and its replacement line
  (String, String)
getPragma [] = error "Ormolu.Preprocess.getPragma: input must not be empty"
getPragma s@(x : xs)
  | "#-}" `L.isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
    let (prag, remline) = getPragma xs
     in (x : prag, ' ' : remline)

-- | Canonical enable marker.
enableMarker :: String
enableMarker = "{- ORMOLU_ENABLE -}"

-- | Canonical disable marker.
disableMarker :: String
disableMarker = "{- ORMOLU_DISABLE -}"

-- | Return 'True' if the given string is an enabling marker.
isOrmoluEnable :: String -> Bool
isOrmoluEnable = magicComment "ORMOLU_ENABLE"

-- | Return 'True' if the given string is a disabling marker.
isOrmoluDisable :: String -> Bool
isOrmoluDisable = magicComment "ORMOLU_DISABLE"

-- | Construct a function for whitespace-insensitive matching of string.
magicComment ::
  -- | What to expect
  String ->
  -- | String to test
  String ->
  -- | Whether or not the two strings watch
  Bool
magicComment expected s0 = isJust $ do
  let trim = dropWhile isSpace
  s1 <- trim <$> L.stripPrefix "{-" (trim s0)
  s2 <- trim <$> L.stripPrefix expected s1
  s3 <- L.stripPrefix "-}" s2
  guard (all isSpace s3)
