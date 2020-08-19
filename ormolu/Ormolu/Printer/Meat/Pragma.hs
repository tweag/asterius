{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Pretty-printing of language pragmas.
module Ormolu.Printer.Meat.Pragma
  ( p_pragmas,
  )
where

import Control.Monad
import Data.Char (isUpper)
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma (Pragma (..))
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import SrcLoc

-- | Pragma classification.
data PragmaTy
  = Language LanguagePragmaClass
  | OptionsGHC
  | OptionsHaddock
  deriving (Eq, Ord)

-- | Language pragma classification.
--
-- The order in which language pragmas are put in the input sometimes
-- matters. This is because some language extensions can enable other
-- extensions, yet the extensions coming later in the list have the ability
-- to change it. So here we classify all extensions by assigning one of the
-- four groups to them. Then we only sort inside of the groups.
--
-- 'Ord' instance of this data type is what affects the sorting.
--
-- See also: <https://github.com/tweag/ormolu/issues/404>
data LanguagePragmaClass
  = -- | All other extensions
    Normal
  | -- | Extensions starting with "No"
    Disabling
  | -- | Extensions that should go after everything else
    Final
  deriving (Eq, Ord)

-- | Print a collection of 'Pragma's with their associated comments.
p_pragmas :: [([RealLocated Comment], Pragma)] -> R ()
p_pragmas ps = do
  let prepare = L.sortOn snd . L.nub . concatMap analyze
      analyze = \case
        (cs, PragmaLanguage xs) ->
          let f x = (cs, (Language (classifyLanguagePragma x), x))
           in f <$> xs
        (cs, PragmaOptionsGHC x) -> [(cs, (OptionsGHC, x))]
        (cs, PragmaOptionsHaddock x) -> [(cs, (OptionsHaddock, x))]
  forM_ (prepare ps) $ \(cs, (pragmaTy, x)) ->
    p_pragma cs pragmaTy x

p_pragma :: [RealLocated Comment] -> PragmaTy -> String -> R ()
p_pragma comments ty x = do
  forM_ comments $ \(L l comment) -> do
    spitCommentNow l comment
    newline
  txt "{-# "
  txt $ case ty of
    Language _ -> "LANGUAGE"
    OptionsGHC -> "OPTIONS_GHC"
    OptionsHaddock -> "OPTIONS_HADDOCK"
  space
  txt (T.pack x)
  txt " #-}"
  newline

-- | Classify a 'LanguagePragma'.
classifyLanguagePragma :: String -> LanguagePragmaClass
classifyLanguagePragma = \case
  "ImplicitPrelude" -> Final
  "CUSKs" -> Final
  str ->
    case splitAt 2 str of
      ("No", rest) ->
        case listToMaybe rest of
          Nothing -> Normal
          Just x ->
            if isUpper x
              then Disabling
              else Normal
      _ -> Normal
