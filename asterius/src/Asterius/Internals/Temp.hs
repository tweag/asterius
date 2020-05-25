-- |
-- Module      :  Asterius.Internals.Temp
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Utilities for creating and using temporary files and directories.
module Asterius.Internals.Temp
  ( temp,
    withTempDir,
  )
where

import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory
import System.IO

-- | Create a temporary file using a file name template.
temp :: FilePath -> IO FilePath
temp p = do
  tmpdir <- getTemporaryDirectory
  (r, h) <- openBinaryTempFile tmpdir p
  hClose h
  pure r

-- | Create a new temporary directory inside the current directory for
-- temporary files (e.g. as specified by variable @TMP@, @TEMP@, etc.), making
-- use of the given directory name template. The temporary directory is
-- automatically deleted after use.
withTempDir :: String -> (FilePath -> IO r) -> IO r
withTempDir t c = do
  tmpdir <- getTemporaryDirectory
  withTempDirectory silent tmpdir t c
