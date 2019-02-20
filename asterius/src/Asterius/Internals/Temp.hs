module Asterius.Internals.Temp
  ( temp
  ) where

import System.Directory
import System.IO

temp :: FilePath -> IO FilePath
temp p = do
  tmpdir <- getTemporaryDirectory
  (r, h) <- openBinaryTempFile tmpdir p
  hClose h
  pure r
