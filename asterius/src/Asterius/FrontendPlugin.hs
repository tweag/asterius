{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Asterius.FrontendPlugin
  ( OriginalArgs(..)
  , frontendPlugin
  ) where

import Asterius.BuildInfo
import Asterius.CmmOrphans
import Asterius.IR
import Asterius.Unexported
import Bindings.Binaryen.Raw
import Control.Monad
import Data.Binary
import Data.List.Extra
import qualified Data.Serialize.Cereal.Utils as C
import DriverPhases
import DriverPipeline
import GHC
import GhcPlugins hiding ((<>))
import Hooks
import PipelineMonad
import qualified Stream
import System.Directory
import System.FilePath
import UnliftIO
import UnliftIO.Environment
import UnliftIO.Process

data OriginalArgs = OriginalArgs
  { args :: [String]
  , mode :: GhcMode
  }

instance Binary OriginalArgs where
  put OriginalArgs {..} = do
    put args
    put $
      case mode of
        CompManager -> 0 :: Word8
        OneShot -> 1
        MkDepend -> 2
  get = do
    ss <- get
    (t :: Word8) <- get
    pure
      OriginalArgs
        { args = ss
        , mode =
            case t of
              0 -> CompManager
              1 -> OneShot
              2 -> MkDepend
              _ -> undefined
        }

frontendPlugin :: FrontendPlugin
frontendPlugin =
  defaultFrontendPlugin
    { frontend =
        \_ targets -> do
          liftIO $ print c_BinaryenTypeInt64
          OriginalArgs {..} <-
            liftIO $ do
              p <- getEnv "ASTERIUS_ORIGINAL_ARGS_PATH"
              decodeFile p
          if all isHaskellishTarget targets
            then do
              (already_run_query, obj_get, obj_put) <-
                liftIO $ do
                  already_run_set_ref <- newIORef emptyModuleSet
                  obj_map_ref <- newIORef emptyModuleEnv
                  obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
                  let obj_fn Module {..} =
                        obj_topdir </> unitIdString moduleUnitId </>
                        foldr1
                          (</>)
                          (wordsBy (== '.') (moduleNameString moduleName)) <.>
                        "asterius_o"
                  pure
                    ( \k ->
                        liftIO $
                        atomicModifyIORef' already_run_set_ref $ \s ->
                          (extendModuleSet s k, k `elemModuleSet` s)
                    , \f k -> do
                        m' <- readIORef obj_map_ref
                        case lookupModuleEnv m' k of
                          Just v -> pure v
                          _ -> do
                            v <- f (obj_fn k)
                            atomicModifyIORef' obj_map_ref $ \m ->
                              (extendModuleEnv m k v, ())
                            pure v
                    , \f k v -> do
                        let fn = obj_fn k
                        createDirectoryIfMissing True $ takeDirectory fn
                        f fn v
                        atomicModifyIORef' obj_map_ref $ \m ->
                          (extendModuleEnv m k v, ()))
              dflags' <- getSessionDynFlags
              void $
                setSessionDynFlags
                  dflags'
                    { ghcMode = mode
                    , hooks =
                        emptyHooks
                          { runPhaseHook =
                              Just $ \phase_plus input_fn dflags ->
                                case phase_plus of
                                  HscOut src_flavour _ (HscRecomp cgguts mod_summary) -> do
                                    let key = ms_mod mod_summary
                                    already_run <- already_run_query key
                                    if already_run
                                      then runPhase phase_plus input_fn dflags
                                      else do
                                        let hsc_lang = hscTarget dflags
                                            next_phase =
                                              hscPostBackendPhase
                                                dflags
                                                src_flavour
                                                hsc_lang
                                        output_fn <-
                                          phaseOutputFilename next_phase
                                        PipeState {hsc_env = hsc_env'} <-
                                          getPipeState
                                        ((outputFilename, mStub, foreign_files), rawcmms) <-
                                          liftIO $
                                          hscGenHardCode'
                                            hsc_env'
                                            cgguts
                                            mod_summary
                                            output_fn
                                        stub_o <-
                                          liftIO
                                            (mapM (compileStub hsc_env') mStub)
                                        foreign_os <-
                                          liftIO $
                                          mapM
                                            (uncurry (compileForeign hsc_env'))
                                            foreign_files
                                        setForeignOs
                                          (maybe [] return stub_o ++ foreign_os)
                                        liftIO $ do
                                          setDynFlagsRef dflags
                                          rawcmms_list <-
                                            concat <$> Stream.collect rawcmms
                                          obj_put C.encodeFile key $
                                            foldl'
                                              (\tot b ->
                                                 toAModule dflags b <> tot)
                                              mempty
                                              rawcmms_list
                                          am <- obj_get C.decodeFile key
                                          print
                                            (moduleName key, length $ show am)
                                        return
                                          (RealPhase next_phase, outputFilename)
                                  _ -> runPhase phase_plus input_fn dflags
                          }
                    }
              sequenceA [guessTarget t f | (t, f) <- targets] >>= setTargets
              sf <- load LoadAllTargets
              case sf of
                Succeeded -> pure ()
                Failed -> throwString "GHC.load returned Failed."
            else liftIO $ callProcess ghc args
    }
