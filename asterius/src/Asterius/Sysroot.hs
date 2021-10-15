module Asterius.Sysroot where

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

{-# NOINLINE sysroot #-}
sysroot :: FilePath
sysroot = unsafePerformIO $ do
  mp <- getEnv "AHC_LIBDIR"
  case mp of
    Nothing -> fail "AHC_LIBDIR is not set"
    Just s -> canonicalizePath s
