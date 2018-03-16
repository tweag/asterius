{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.GHC.Toolkit.FrontendPlugin
  ( frontendPluginFromCompiler
  ) where

import Control.Monad
import Data.List
import DriverPhases
import DriverPipeline
import GHC
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Hooks
import Panic

frontendPluginFromCompiler :: Ghc Compiler -> FrontendPlugin
frontendPluginFromCompiler init_c =
  defaultFrontendPlugin
    { frontend =
        \_ targets -> do
          let (hs_targets, non_hs_targets) =
                partition isHaskellishTarget targets
          if null hs_targets
            then do
              dflags <- getSessionDynFlags
              void $ setSessionDynFlags dflags {ghcLink = NoLink}
              env <- getSession
              liftIO $ oneShot env StopLn targets
            else do
              c <- init_c
              h <- liftIO $ hooksFromCompiler c
              env <- getSession
              o_files <-
                liftIO $ traverse (compileFile env StopLn) non_hs_targets
              dflags <- getSessionDynFlags
              void $
                setSessionDynFlags
                  dflags
                    { ghcMode = CompManager
                    , ldInputs = map (FileOption "") o_files ++ ldInputs dflags
                    , hooks = h
                    }
              traverse (uncurry GHC.guessTarget) hs_targets >>= setTargets
              ok_flag <- load LoadAllTargets
              when (failed ok_flag) $
                liftIO $ throwGhcExceptionIO $ Panic "GHC.load returned Failed."
    }
