{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.FakeGHC
  ( FakeGHCOptions (..),
    fakeGHCMain,
  )
where

import Control.Exception
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
          -- If the output seems to be a Cabal setup executable, we just call
          -- the host GHC to compile it, using only the host GHC's own global
          -- pkgdb. This is an ugly workaround to support Cabal packages with
          -- custom Setup.hs scripts, see #342 and related PR for details.
          case GHC.outputFile dflags1 of
            Just p | seemsToBeCabalSetup p ->
              liftIO
                $ catch
                  ( callProcess
                      hostGHC
                      ( ["--make", "-o", p, "-threaded"]
                          <> map GHC.unLoc fileish_args
                      )
                  )
                $ \(_ :: IOError) -> do
                  writeFile
                    (GHC.unLoc (head fileish_args))
                    "import Distribution.Simple\nmain = defaultMain\n"
                  callProcess
                    hostGHC
                    ( ["--make", "-o", p, "-threaded"]
                        <> map GHC.unLoc fileish_args
                    )
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

-- | Uses a heuristic to determine if a GHC output path looks like the Cabal
-- setup file: if the path matches "**/dist/setup/setup*", then we consider the
-- output to be the Cabal setup executable being compiled by cabal-install at
-- the moment. False positives are possible but unlikely.
seemsToBeCabalSetup :: FilePath -> Bool
seemsToBeCabalSetup p = case reverse $ splitDirectories p of
  (('s' : 'e' : 't' : 'u' : 'p' : _) : "setup" : "dist" : _) -> True
  _ -> False

hostGHC :: FilePath
hostGHC = "ghc"
