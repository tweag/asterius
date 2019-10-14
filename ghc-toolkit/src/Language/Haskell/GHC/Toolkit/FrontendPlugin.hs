module Language.Haskell.GHC.Toolkit.FrontendPlugin
  ( makeFrontendPlugin,
  )
where

import Config
import Control.Monad
import Data.List
import DriverPhases
import DriverPipeline
import GHC
import GhcPlugins hiding ((<>))
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Hooks
import Language.Haskell.GHC.Toolkit.Orphans.Show
import Panic

makeFrontendPlugin :: Ghc Compiler -> FrontendPlugin
makeFrontendPlugin init_c =
  defaultFrontendPlugin
    { frontend = \_ targets -> do
        c <- init_c
        dflags <- getSessionDynFlags
        h <- liftIO $ hooksFromCompiler c (hooks dflags)
        let (hs_targets, non_hs_targets) = partition isHaskellishTarget targets
        if null hs_targets
          then do
            void $
              setSessionDynFlags
                dflags
                  { integerLibrary = IntegerSimple,
                    tablesNextToCode = False,
                    hooks = h
                  }
            getSessionDynFlags >>= setDynFlagsRef
            env <- getSession
            liftIO $ oneShot env StopLn targets
          else do
            void $
              setSessionDynFlags
                dflags
                  { integerLibrary = IntegerSimple,
                    tablesNextToCode = False,
                    hooks = h
                  }
            env <- getSession
            o_files <- liftIO $ traverse (compileFile env StopLn) non_hs_targets
            dflags' <- getSessionDynFlags
            void $
              setSessionDynFlags
                dflags'
                  { ghcMode = CompManager,
                    ldInputs = map (FileOption "") o_files ++ ldInputs dflags'
                  }
            traverse (uncurry GHC.guessTarget) hs_targets >>= setTargets
            getSessionDynFlags >>= setDynFlagsRef
            ok_flag <- load LoadAllTargets
            when (failed ok_flag) $ liftIO $ throwGhcExceptionIO $
              Panic
                "GHC.load returned Failed."
    }
