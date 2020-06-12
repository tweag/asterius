{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Ar
  ( loadAr,
  )
where

import qualified Ar as GHC
import Asterius.Binary.ByteString
import Asterius.Types
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import qualified IfaceEnv as GHC

-- | Load the contents of an archive (@.a@) file as an 'AsteriusCachedModule'.
-- 'loadAr' ignores (@.o@) files in the archive that cannot be parsed. Also,
-- the metadata of the contained files are ignored (@ahc-ar@ always sets them
-- to default values anyway). If the metadata are really needed, make sure to
-- update @ahc-ar@ to generate non-default values for them.
loadAr :: GHC.NameCacheUpdater -> FilePath -> IO AsteriusCachedModule
loadAr ncu p = do
  GHC.Archive entries <- gnuLoadAr p -- GHC.loadAr p
  foldlM
    ( \acc GHC.ArchiveEntry {..} -> tryGetBS ncu filedata >>= \case
        Left _ -> pure acc
        Right m -> pure $ m <> acc
    )
    mempty
    entries

-- | Archives have numeric values padded with '\x20' to the right.
getPaddedInt :: BS.ByteString -> Int
getPaddedInt = read . CBS.unpack . CBS.takeWhile (/= '\x20')

-- | GNU Archives feature a special '//' entry that contains the
-- extended names. Those are referred to as /<num>, where num is the
-- offset into the '//' entry.
-- In addition, filenames are terminated with '/' in the archive.
getGNUArchEntries :: Maybe BS.ByteString -> Get [GHC.ArchiveEntry]
getGNUArchEntries extInfo = do
  is_empty <- isEmpty
  if is_empty
    then return []
    else do
      name <- getByteString 16
      time <- getPaddedInt <$> getByteString 12
      own <- getPaddedInt <$> getByteString 6
      grp <- getPaddedInt <$> getByteString 6
      mode <- getPaddedInt <$> getByteString 8
      st_size <- getPaddedInt <$> getByteString 10
      end <- getByteString 2
      when (end /= "\x60\x0a") $
        fail
          ( "[GNU Archive] Invalid archive header end marker for name: "
              ++ CBS.unpack name
          )
      file <- getByteString st_size
      -- data sections are two byte aligned (see Trac #15396)
      when (odd st_size) $
        void (getByteString 1)
      real_name <-
        return . CBS.unpack $
          if CBS.unpack (CBS.take 1 name) == "/"
            then case CBS.takeWhile (/= ' ') name of
              "/" -> "/" -- symbol table
              "//" -> "//" -- extended file names table
              stripped_name -> getExtName extInfo (read . CBS.unpack $ CBS.drop 1 stripped_name)
            else CBS.takeWhile (/= '/') name
      case real_name of
        "/" -> getGNUArchEntries extInfo
        "//" -> getGNUArchEntries (Just file)
        _ -> (GHC.ArchiveEntry real_name time own grp mode st_size file :) <$> getGNUArchEntries extInfo
  where
    getExtName :: Maybe BS.ByteString -> Int -> BS.ByteString
    getExtName Nothing _ = error "Invalid extended filename reference."
    getExtName (Just info) offset = CBS.takeWhile (/= '/') $ CBS.drop offset info

getArchMagic :: Get ()
getArchMagic = do
  magic <- liftM CBS.unpack $ getByteString 8
  when (magic /= "!<arch>\n")
    $ fail
    $ "Invalid magic number " ++ show magic

getArch :: Get GHC.Archive
getArch = GHC.Archive <$> do
  getArchMagic
  getGNUArchEntries Nothing

parseAr :: BS.ByteString -> GHC.Archive
parseAr = runGet getArch . LBS.fromChunks . pure

gnuLoadAr :: FilePath -> IO GHC.Archive
gnuLoadAr fp = parseAr <$> BS.readFile fp
