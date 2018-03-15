{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Toolkit.BuildInfo
  ( bootLibsPath
  , ghc
  , ghcPkg
  , ghcLibDir
  ) where

import Data.Char
import Data.Maybe
import Language.Haskell.TH.Syntax
import Paths_ghc_toolkit
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe
import System.Process

bootLibsPath :: FilePath
bootLibsPath =
  $(do datadir <- runIO getDataDir
       liftString $ datadir </> "boot-libs")

{-# NOINLINE ghc #-}
ghc :: FilePath
ghc =
  unsafePerformIO $
  fromMaybe $(runIO getExecutablePath >>= liftString) <$> lookupEnv "NIX_GHC"

{-# NOINLINE ghcPkg #-}
ghcPkg :: FilePath
ghcPkg =
  unsafePerformIO $
  fromMaybe
    $(runIO getExecutablePath >>=
      liftString . (</> ("ghc-pkg" <.> exeExtension)) . takeDirectory) <$>
  lookupEnv "NIX_GHCPKG"

{-# NOINLINE ghcLibDir #-}
ghcLibDir :: FilePath
ghcLibDir =
  unsafePerformIO $
  fromMaybe
    $(runIO
        (do _ghc <- getExecutablePath
            reverse . dropWhile isSpace . reverse <$>
              readProcess _ghc ["--print-libdir"] "") >>=
      liftString) <$>
  lookupEnv "NIX_GHC_LIBDIR"
