{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Toolkit.BuildInfo
  ( bootLibsPath
  , ghc
  , ghcPkg
  , ghcLibDir
  ) where

import Data.Binary
import qualified Data.Map as M
import Data.Maybe
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Language.Haskell.TH.Syntax
import Paths_ghc_toolkit
import System.Environment
import System.FilePath
import System.IO.Unsafe

{-# NOINLINE bootLibsPath #-}
bootLibsPath :: FilePath
bootLibsPath = unsafePerformIO $ (</> "boot-libs") <$> getDataDir

{-# NOINLINE ghc #-}
ghc :: FilePath
ghc =
  unsafePerformIO $
  fromMaybe
    $(runIO
        (do lbi <- decodeFile ".ghc-toolkit.buildinfo"
            let Just p = lookupProgram ghcProgram $ withPrograms lbi
            pure $ locationPath $ programLocation p) >>=
      liftString) <$>
  lookupEnv "NIX_GHC"

{-# NOINLINE ghcPkg #-}
ghcPkg :: FilePath
ghcPkg =
  unsafePerformIO $
  fromMaybe
    $(runIO
        (do lbi <- decodeFile ".ghc-toolkit.buildinfo"
            let Just p = lookupProgram ghcPkgProgram $ withPrograms lbi
            pure $ locationPath $ programLocation p) >>=
      liftString) <$>
  lookupEnv "NIX_GHCPKG"

{-# NOINLINE ghcLibDir #-}
ghcLibDir :: FilePath
ghcLibDir =
  unsafePerformIO $
  fromMaybe
    $(runIO
        (do lbi <- decodeFile ".ghc-toolkit.buildinfo"
            pure $ compilerProperties (compiler lbi) M.! "LibDir") >>=
      liftString) <$>
  lookupEnv "NIX_GHC_LIBDIR"
