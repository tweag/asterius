{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.ObjectStore
  ( StoreConfig(..)
  , Store(..)
  , newStore
  ) where

import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as M
import Module
import System.Directory
import System.FilePath

data StoreConfig m a = StoreConfig
  { storeTopDir, objectExt, globalFile :: FilePath
  , cacheObject :: Bool
  , rawRead :: FilePath -> m a
  , rawWrite :: FilePath -> a -> m ()
  }

data Store m a = Store
  { objectRead :: Module -> m a
  , objectWrite :: Module -> a -> m ()
  , globalFile :: FilePath
  , globalRead :: m a
  , globalWrite :: a -> m ()
  }

newStore :: MonadIO m => StoreConfig m a -> m (Store m a)
newStore StoreConfig {..} =
  if cacheObject
    then do
      obj_map <- liftIO $ newIORef M.empty
      pure
        Store
          { objectRead =
              \m -> do
                om <- liftIO $ readIORef obj_map
                case M.lookup m om of
                  Just r -> pure r
                  _ -> do
                    r <- rawRead $ p m
                    liftIO $
                      atomicModifyIORef' obj_map $ \om' ->
                        (M.insert m r om', ())
                    pure r
          , objectWrite = w
          , globalFile = globalFile
          , globalRead = rawRead (storeTopDir </> globalFile)
          , globalWrite = rawWrite (storeTopDir </> globalFile)
          }
    else pure
           Store
             { objectRead = rawRead . p
             , objectWrite = w
             , globalFile = globalFile
             , globalRead = rawRead (storeTopDir </> globalFile)
             , globalWrite = rawWrite (storeTopDir </> globalFile)
             }
  where
    w m a = do
      let x = p m
      liftIO $ createDirectoryIfMissing True $ takeDirectory x
      rawWrite x a
    p Module {..} =
      storeTopDir </> unitIdString moduleUnitId </>
      foldr1
        (</>)
        (foldr
           (\case
              '.' -> ([] :)
              c -> \(x:xs) -> (c : x) : xs)
           [[]]
           (moduleNameString moduleName)) <.>
      objectExt
