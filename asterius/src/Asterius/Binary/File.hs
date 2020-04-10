{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Binary.File
  ( putFile,
    getFile,
    tryGetFile,
  )
where

import qualified BinIface as GHC
import qualified Binary as GHC
import Control.Exception
import qualified IfaceEnv as GHC

putFile :: GHC.Binary a => FilePath -> a -> IO ()
putFile p a = do
  bh <- GHC.openBinMem 1048576
  GHC.putWithUserData (const (pure ())) bh a
  GHC.writeBinMem bh p

getFile :: GHC.Binary a => GHC.NameCacheUpdater -> FilePath -> IO a
getFile ncu p = do
  bh <- GHC.readBinMem p
  GHC.getWithUserData ncu bh

tryGetFile ::
  GHC.Binary a =>
  GHC.NameCacheUpdater ->
  FilePath ->
  IO (Either SomeException a)
tryGetFile ncu = try . getFile ncu
