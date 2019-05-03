{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Iserv.State
  ( IservModules(..)
  , IservState(..)
  , withIservState
  , addArchive
  , addObj
  , createSplice
  , loadSplice
  ) where

import Asterius.Ar
import Asterius.Internals
import Asterius.Types
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified GHCi.RemoteTypes as GHC
import Language.JavaScript.Inline.Core
import Prelude hiding (IO)
import Unsafe.Coerce

data IservModules = IservModules
  { iservArchives :: AsteriusModule
  , iservObjs :: Map FilePath AsteriusModule
  , iservSplices :: IntMap (AsteriusEntitySymbol, AsteriusModule)
  }

data IservState = IservState
  { iservModulesRef :: IORef IservModules
  , iservJSSession :: JSSession
  }

withIservState :: (IservState -> IO r) -> IO r
withIservState c = do
  mods_ref <-
    newIORef
      IservModules
        {iservArchives = mempty, iservObjs = mempty, iservSplices = mempty}
  withJSSession defJSSessionOpts {nodeStdErrInherit = True} $ \s ->
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

createSplice ::
     IservState -> LBS.ByteString -> LBS.ByteString -> IO GHC.HValueRef
createSplice IservState {..} sym_buf m_buf = do
  JSVal v <- alloc iservJSSession LBS.empty
  modifyIORef' iservModulesRef $ \s ->
    s
      { iservSplices =
          IntMap.insert v (decode sym_buf, decode m_buf) $ iservSplices s
      }
  pure $ unsafeCoerce $ GHC.RemotePtr $ fromIntegral v

loadSplice ::
     IservState -> GHC.HValueRef -> IO (AsteriusEntitySymbol, AsteriusModule)
loadSplice IservState {..} hv = do
  IservModules {..} <- readIORef iservModulesRef
  (sym, splice_m) <-
    maybe (fail $ "Non-existent splice " <> show hv) pure $
    IntMap.lookup i iservSplices
  pure (sym, splice_m <> fold iservObjs <> iservArchives)
  where
    GHC.RemotePtr p = unsafeCoerce hv
    i = fromIntegral p
