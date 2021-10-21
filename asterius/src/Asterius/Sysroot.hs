module Asterius.Sysroot
  ( srcDir,
  )
where

import System.Directory
import System.Environment.Blank
import System.IO.Unsafe

{-# NOINLINE srcDir #-}
srcDir :: FilePath
srcDir = unsafePerformIO $ do
  mp <- getEnv "AHC_SRCDIR"
  case mp of
    Nothing -> fail "AHC_SRCDIR is not set"
    Just s -> canonicalizePath s
