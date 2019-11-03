{-# LANGUAGE OverloadedStrings #-}

module Asterius.Internals.Session
  ( fakeSession,
  )
where

import qualified Asterius.BuildInfo as A
import qualified Config as GHC
import Control.Monad
import Data.Foldable
import Data.List
import qualified DynFlags as GHC
import qualified GHC
import System.Environment.Blank
import System.FilePath

fakeSession :: GHC.Ghc r -> IO r
fakeSession m = do
  ks <-
    filter (\k -> ("GHC_" `isPrefixOf` k) || "HASKELL_" `isPrefixOf` k)
      . map fst
      <$> getEnvironment
  for_ ks unsetEnv
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
    $ GHC.runGhc (Just (A.dataDir </> ".boot" </> "asterius_lib"))
    $ do
      dflags0 <- GHC.getSessionDynFlags
      void $
        GHC.setSessionDynFlags
          dflags0
            { GHC.ghcMode = GHC.CompManager,
              GHC.hscTarget = GHC.HscAsm,
              GHC.integerLibrary = GHC.IntegerSimple,
              GHC.tablesNextToCode = False
            }
      m
