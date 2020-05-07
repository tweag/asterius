{-# LANGUAGE CPP #-}

module NofibUtils where

import Data.Char (ord)
import Data.List (foldl')

import System.Environment (getArgs)

-- | A very simple hash function so that we don't have to store and compare
-- huge output files.
hash :: String -> Int
hash = foldl' (\acc c -> ord c + acc*31) 0

-- | Using @salt xs@ on an loop-invariant @xs@ inside a loop prevents the
-- compiler from floating out the input parameter.
#ifdef __GLASGOW_HASKELL__
salt :: a -> IO a
salt = pure
{-# NOINLINE salt #-}
#else
salt :: [a] -> IO [a]
-- this won't work with real/lift, but I can't think of another way
salt xs = do
  s <- length <$> getArgs
  -- Invariant: There are less than 'maxBound' parameters passed to the
  --            executable, otherwise this isn't really 'pure'
  --            anymore.
  pure (take (max (maxBound - 1) s) xs)
#endif