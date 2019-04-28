{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Iserv.State
  ( IservModules(..)
  , IservState(..)
  , withIservState
  , addArchive
  , addObj
  ) where

import Asterius.Ar
import Asterius.Internals
import Asterius.Types
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Language.JavaScript.Inline.Core
import Prelude hiding (IO)

data IservModules = IservModules
  { iservArchives :: AsteriusModule
  , iservObjs :: Map FilePath AsteriusModule
  }

data IservState = IservState
  { iservModulesRef :: IORef IservModules
  , iservJSSession :: JSSession
  }

withIservState :: (IservState -> IO r) -> IO r
withIservState c = do
  mods_ref <- newIORef IservModules {iservArchives = mempty, iservObjs = mempty}
  withJSSession defJSSessionOpts $ \s ->
    c IservState {iservModulesRef = mods_ref, iservJSSession = s}

addArchive :: IservState -> FilePath -> IO ()
addArchive IservState {..} p = do
  m <- loadAr p
  modifyIORef' iservModulesRef $ \s -> s {iservArchives = m <> iservArchives s}

addObj :: IservState -> FilePath -> IO ()
addObj IservState {..} p = do
  m <- decodeFile p
  modifyIORef' iservModulesRef $ \s ->
    s {iservObjs = Map.insert p m $ iservObjs s}
