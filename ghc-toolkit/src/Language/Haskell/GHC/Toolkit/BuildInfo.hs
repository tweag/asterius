module Language.Haskell.GHC.Toolkit.BuildInfo
  ( bootLibsPath,
    sandboxGhcLibDir,
    dataDir,
  )
where

import Paths_ghc_toolkit
import System.FilePath
import System.Environment
import System.IO.Unsafe

bootLibsPath :: FilePath
bootLibsPath = dataDir </> "boot-libs"

sandboxGhcLibDir :: FilePath
sandboxGhcLibDir = dataDir </> "ghc-libdir"

getDataDirFromEnv = do
  e <- getEnvironment
  let l = [(k, v) | (k, v) <- e, k == "ASTERIUS_DATA_DIR"]
  case l of
   [] -> do
     error "tmp no env"
     datadir <- Paths_ghc_toolkit.getDataDir
     pure datadir
   [(_, datadir)] ->
     pure datadir

{-# NOINLINE dataDir #-}
dataDir :: FilePath
-- dataDir = unsafePerformIO getDataDir
dataDir = unsafePerformIO getDataDirFromEnv
