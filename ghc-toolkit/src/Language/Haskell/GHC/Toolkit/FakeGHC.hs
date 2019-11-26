{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.FakeGHC
  ( FakeGHCOptions (..),
    fakeGHCMain,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.List
import qualified DynFlags as GHC
import qualified GHC
import qualified Plugins as GHC
import System.Environment.Blank
import System.FilePath
import System.Process

data FakeGHCOptions
  = FakeGHCOptions
      { ghc, ghcLibDir :: FilePath,
        frontendPlugin :: GHC.FrontendPlugin
      }

fakeGHCMain :: FakeGHCOptions -> IO ()
fakeGHCMain FakeGHCOptions {..} = do
  ks <-
    filter (\k -> ("GHC_" `isPrefixOf` k) || "HASKELL_" `isPrefixOf` k)
      . map fst
      <$> getEnvironment
  for_ ks unsetEnv
  args0 <- getArgs
  let (minusB_args, args1) = partition ("-B" `isPrefixOf`) args0
      new_ghc_libdir = case minusB_args of
        [minusB_arg] -> drop 2 minusB_arg
        _ -> ghcLibDir
  case partition (== "--make") args1 of
    ([], _) -> callProcess ghc $ ("-B" <> new_ghc_libdir) : args1
    (_, args2) ->
      GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
        $ GHC.runGhc (Just new_ghc_libdir)
        $ do
          dflags0 <- GHC.getSessionDynFlags
          (dflags1, fileish_args, _) <-
            GHC.parseDynamicFlags
              dflags0
              (map GHC.noLoc args2)
          case GHC.outputFile dflags1 of
            Just p
              | seemsToBeCabalSetup p ->
                liftIO
                  $ callProcess ghc
                  $ ["--make", "-o", p, "-threaded"]
                    <> map GHC.unLoc fileish_args
            _ -> do
              void $
                GHC.setSessionDynFlags
                  dflags1
                    { GHC.ghcMode = GHC.CompManager,
                      GHC.hscTarget = GHC.HscAsm
                    }
              GHC.frontend
                frontendPlugin
                []
                [(GHC.unLoc m, Nothing) | m <- fileish_args]

seemsToBeCabalSetup :: FilePath -> Bool
seemsToBeCabalSetup p = case reverse $ splitDirectories p of
  (('s' : 'e' : 't' : 'u' : 'p' : _) : "setup" : "dist" : _) -> True
  _ -> False
