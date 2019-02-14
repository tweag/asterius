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
import System.Environment.Blank
import System.Process

data FakeGHCOptions = FakeGHCOptions
  { ghc, ghcLibDir :: FilePath
  , frontendPlugin :: GHC.FrontendPlugin
  }

fakeGHCMain :: FakeGHCOptions -> IO ()
fakeGHCMain FakeGHCOptions {..} = do
  unsetEnv "GHC_PACKAGE_PATH"
  args0 <- getArgs
  let (minusB_args, args1) = partition ("-B" `isPrefixOf`) args0
      new_ghc_libdir =
        case minusB_args of
          [minusB_arg] -> drop 2 minusB_arg
          _ -> ghcLibDir
  case partition (== "--make") args1 of
    ([], _) -> callProcess ghc $ ("-B" <> new_ghc_libdir) : args1
    (_, args2) ->
      GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
      GHC.runGhc (Just new_ghc_libdir) $ do
        dflags0 <- GHC.getSessionDynFlags
        (dflags1, fileish_args, _) <-
          GHC.parseDynamicFlags dflags0 (map GHC.noLoc args2)
        void $
          GHC.setSessionDynFlags
            dflags1
              { GHC.ghcMode = GHC.CompManager
              , GHC.ghcLink = GHC.NoLink
              , GHC.hscTarget = GHC.HscAsm
              }
        GHC.frontend
          frontendPlugin
          []
          [(GHC.unLoc m, Nothing) | m <- fileish_args]
