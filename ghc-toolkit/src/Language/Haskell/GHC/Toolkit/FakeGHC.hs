{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.FakeGHC
  ( FakeGHCOptions(..)
  , fakeGHCMain
  ) where

import Control.Monad
import Data.List
import qualified DynFlags as GHC
import qualified GHC
import qualified Plugins as GHC
import System.Environment
import System.Process

data FakeGHCOptions = FakeGHCOptions
  { ghc, ghcLibDir :: FilePath
  , frontendPlugin :: GHC.FrontendPlugin
  }

fakeGHCMain :: FakeGHCOptions -> IO ()
fakeGHCMain FakeGHCOptions {..} = do
  args0 <- getArgs
  case partition (== "--make") args0 of
    ([], _) -> callProcess ghc args0
    (_, args1) ->
      GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
      GHC.runGhc (Just ghcLibDir) $ do
        dflags0 <- GHC.getSessionDynFlags
        (dflags1, fileish_args, _) <-
          GHC.parseDynamicFlags dflags0 (map GHC.noLoc args1)
        void $
          GHC.setSessionDynFlags
            dflags1
              { GHC.ghcMode = GHC.CompManager
              , GHC.ghcLink = GHC.NoLink
              , GHC.hscTarget = GHC.HscAsm
              , GHC.verbosity = 1
              }
        GHC.frontend
          frontendPlugin
          []
          [(GHC.unLoc m, Nothing) | m <- fileish_args]
