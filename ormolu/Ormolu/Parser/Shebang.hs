{-# LANGUAGE DeriveDataTypeable #-}

-- | A module for dealing with shebangs.
module Ormolu.Parser.Shebang
  ( Shebang (..),
    extractShebangs,
    isShebang,
  )
where

import Data.Data (Data)
import qualified Data.List as L
import SrcLoc

-- | A wrapper for a shebang.
newtype Shebang = Shebang (Located String)
  deriving (Eq, Data)

-- | Extract shebangs from the beginning of a comment stream.
extractShebangs :: [Located String] -> ([Shebang], [Located String])
extractShebangs comments = (Shebang <$> shebangs, rest)
  where
    (shebangs, rest) = span (isShebang . unLoc) comments

-- | Return 'True' if given 'String' is a shebang.
isShebang :: String -> Bool
isShebang str = "#!" `L.isPrefixOf` str
