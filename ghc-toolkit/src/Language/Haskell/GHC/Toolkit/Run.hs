{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Run
  ( Config(..),
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
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Hooks

data Config = Config
  { ghcFlags :: [String],
    ghcLibDir :: FilePath
  }

runCmm :: Config -> [FilePath] -> (FilePath -> CmmIR -> IO ()) -> GHC.Ghc ()
runCmm Config {..} cmm_fns write_obj_cont = do
  dflags <- getSessionDynFlags
  (dflags', _, _) <-
    parseDynamicFlags
      dflags
      $ map noLoc ghcFlags
  h <-
    liftIO $
      hooksFromCompiler
        ( Compiler
            { withHaskellIR = \_ _ _ _ -> pure (),
              withCmmIR = \_ _ ir obj_path -> liftIO $ write_obj_cont obj_path ir
            }
        )
        (hooks dflags')
  void $
    setSessionDynFlags
      dflags'
        { ghcLink = NoLink,
          hooks = h
        }
  env <- getSession
  liftIO $ oneShot env StopLn [(cmm_fn, Just CmmCpp) | cmm_fn <- cmm_fns]
