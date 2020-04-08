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

putFile :: GHC.Binary a => FilePath -> a -> IO ()
putFile p a = do
  bh <- GHC.openBinMem 1048576
  GHC.putWithUserData (const (pure ())) bh a
  GHC.writeBinMem bh p

getFile :: GHC.Binary a => FilePath -> IO a
getFile p = do
  bh <- GHC.readBinMem p
  getWithUserData bh

tryGetFile :: GHC.Binary a => FilePath -> IO (Either SomeException a)
tryGetFile = try . getFile

getWithUserData :: GHC.Binary a => GHC.BinHandle -> IO a
getWithUserData bh' = do
  dict_p <- GHC.get bh'
  data_p <- GHC.tellBin bh'
  GHC.seekBin bh' dict_p
  dict <- GHC.getDictionary bh'
  GHC.seekBin bh' data_p
  bh <- do
    let bh =
          GHC.setUserData bh' $
            GHC.newReadState
              (error "Asterius.Binary.File.getSymtabName")
              (GHC.getDictFastString dict)
    (_ :: GHC.Bin ()) <- GHC.get bh
    pure $ GHC.setUserData bh $
      GHC.newReadState
        (error "Asterius.Binary.File.getWithUserData")
        (GHC.getDictFastString dict)
  GHC.get bh
