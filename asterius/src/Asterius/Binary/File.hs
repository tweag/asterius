{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Binary.File
  ( putFile,
    getFile,
    tryGetFile,
  )
where

import Asterius.Binary.UserData
import qualified Binary as GHC
import Control.Exception

putFile :: GHC.Binary a => FilePath -> a -> IO ()
putFile p a = do
  bh <- GHC.openBinMem 1048576
  putWithUserData (const (pure ())) bh a
  GHC.writeBinMem bh p

getFile :: GHC.Binary a => FilePath -> IO a
getFile p = do
  bh <- GHC.readBinMem p
  getWithUserData bh

tryGetFile :: GHC.Binary a => FilePath -> IO (Either SomeException a)
tryGetFile = try . getFile
