module Asterius.Sysroot where

import qualified Paths_asterius as A
import System.Directory
import System.Environment.Blank
import System.IO.Unsafe

{-# NOINLINE dataDir #-}
dataDir :: FilePath
dataDir = unsafePerformIO $ canonicalizePath =<< A.getDataDir

{-# NOINLINE sysroot #-}
sysroot :: FilePath
sysroot = unsafePerformIO $ do
  mp <- getEnv "AHC_LIBDIR"
  case mp of
    Nothing -> fail "AHC_LIBDIR is not set"
    Just s -> canonicalizePath s
