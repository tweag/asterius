{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Asterius.Iserv.State
  ( IservState(..)
  , initIservState
  , addArchive
  , addObj
  ) where

import Asterius.Ar
import Asterius.Internals
import Asterius.Types
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (IO)

data IservState = IservState
  { iservArchives :: AsteriusModule
  , iservObjs :: Map FilePath AsteriusModule
  }

initIservState :: IO (IORef IservState)
initIservState =
  newIORef $ IservState {iservArchives = mempty, iservObjs = Map.empty}

addArchive :: IORef IservState -> FilePath -> IO ()
addArchive ref p = do
  m <- loadAr p
  modifyIORef' ref $ \s -> s {iservArchives = m <> iservArchives s}

addObj :: IORef IservState -> FilePath -> IO ()
addObj ref p = do
  m <- decodeFile p
  modifyIORef' ref $ \s -> s {iservObjs = Map.insert p m $ iservObjs s}
