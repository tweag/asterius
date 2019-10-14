{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Run
  ( Config,
    ghcFlags,
    ghcLibDir,
    compiler,
    defaultConfig,
    runCmm,
  )
where

import Config
import Control.Monad.IO.Class
import Data.Functor
import DriverPhases
import DriverPipeline
import DynFlags
import GHC
import qualified Language.Haskell.GHC.Toolkit.BuildInfo as BI
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Hooks

data Config
  = Config
      { ghcFlags :: [String],
        ghcLibDir :: FilePath,
        compiler :: Compiler
      }

defaultConfig :: Config
defaultConfig = Config
  { ghcFlags = ["-Wall", "-O"],
    ghcLibDir = BI.ghcLibDir,
    compiler = mempty
  }

runCmm :: Config -> [FilePath] -> (FilePath -> CmmIR -> IO ()) -> GHC.Ghc ()
runCmm Config {..} cmm_fns write_obj_cont = do
  dflags <- getSessionDynFlags
  (dflags', _, _) <-
    parseDynamicFlags
      (dflags `gopt_set` Opt_ForceRecomp `gopt_unset` Opt_KeepOFiles)
      $ map noLoc ghcFlags
  h <-
    liftIO $
      hooksFromCompiler
        ( compiler
            <> mempty
              { withCmmIR = \ir obj_path -> liftIO $ write_obj_cont obj_path ir
              }
        )
        (hooks dflags')
  void $
    setSessionDynFlags
      dflags'
        { ghcMode = OneShot,
          ghcLink = NoLink,
          integerLibrary = IntegerSimple,
          tablesNextToCode = False,
          hooks = h
        }
  env <- getSession
  liftIO $ oneShot env StopLn [(cmm_fn, Just CmmCpp) | cmm_fn <- cmm_fns]
