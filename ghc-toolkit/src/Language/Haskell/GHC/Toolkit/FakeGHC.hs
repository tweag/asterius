{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.FakeGHC
  ( FakeGHCOptions(..)
  , fakeMain
  ) where

import Distribution.Simple.Compiler
import System.Environment
import System.Process

data FakeGHCOptions = FakeGHCOptions
  { ghc :: FilePath
  , pluginModuleName, pluginPackageName :: String
  , packageDBStack :: PackageDBStack
  }

fakeMain :: FakeGHCOptions -> IO ()
fakeMain FakeGHCOptions {..} = do
  args <- getArgs
  callProcess ghc $ do
    arg <- args
    case arg of
      "--make" ->
        ["--frontend", pluginModuleName, "-plugin-package", pluginPackageName] ++
        case registrationPackageDB packageDBStack of
          GlobalPackageDB -> ["-global-package-db"]
          UserPackageDB -> ["-user-package-db"]
          SpecificPackageDB p -> ["-package-db", p]
      _ -> [arg]
