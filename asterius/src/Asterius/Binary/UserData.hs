{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Binary.UserData
  ( GHC.putWithUserData,
    getWithUserData,
  )
where

import qualified BinIface as GHC
import qualified Binary as GHC

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
