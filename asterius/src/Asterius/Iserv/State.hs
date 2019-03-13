{-# LANGUAGE StrictData #-}

module Asterius.Iserv.State
  ( IservState(..)
  , initIservState
  , addArchive
  , addObj
  ) where

import Data.IORef

data IservState = IservState
  { iservArchives, iservObjs :: [FilePath]
  }

initIservState :: IO (IORef IservState)
initIservState = newIORef $ IservState {iservArchives = [], iservObjs = []}

addArchive :: IORef IservState -> FilePath -> IO ()
addArchive ref p =
  modifyIORef' ref $ \s -> s {iservArchives = p : iservArchives s}

addObj :: IORef IservState -> FilePath -> IO ()
addObj ref p = modifyIORef' ref $ \s -> s {iservObjs = p : iservObjs s}
