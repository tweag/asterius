{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Postprocessing for the results of printing.
module Ormolu.Processing.Postprocess
  ( postprocess,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Processing.Common
import qualified Ormolu.Processing.Cpp as Cpp

-- | Postprocess output of the formatter.
postprocess ::
  -- | Desired indentation level
  Int ->
  -- | Input to process
  Text ->
  Text
postprocess indent =
  T.unlines
    . fmap indentLine
    . fmap Cpp.unmaskLine
    . filter (not . magicComment)
    . T.lines
  where
    magicComment (T.stripStart -> x) =
      x == startDisabling || x == endDisabling
    indentLine x = T.replicate indent " " <> x
