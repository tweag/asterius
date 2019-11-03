module Asterius.Internals.Temp
  ( temp,
    withTempDir,
  )
where

import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory
import System.IO

temp :: FilePath -> IO FilePath
temp p = do
  tmpdir <- getTemporaryDirectory
  (r, h) <- openBinaryTempFile tmpdir p
  hClose h
  pure r

withTempDir :: String -> (FilePath -> IO r) -> IO r
withTempDir t c = do
  tmpdir <- getTemporaryDirectory
  withTempDirectory silent tmpdir t c
