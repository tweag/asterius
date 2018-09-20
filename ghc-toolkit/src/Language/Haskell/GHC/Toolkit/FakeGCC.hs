{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.FakeGCC
  ( FakeGCCOptions(..)
  , fakeGCCMain
  ) where

import System.Environment
import System.Process

data FakeGCCOptions = FakeGCCOptions
  { gccPath, ghcLibDir, prependIncludeDir :: FilePath
  }

fakeGCCMain :: FakeGCCOptions -> IO ()
fakeGCCMain FakeGCCOptions {..} = do
  args <- getArgs
  callProcess gccPath $ ("-I" <> prependIncludeDir) : args
