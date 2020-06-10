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
import Control.Applicative
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
  GHC.Archive entries <- ghcLoadAr p -- GHC.loadAr p
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

getBSDArchEntries :: Get [GHC.ArchiveEntry]
getBSDArchEntries = do
  is_empty <- isEmpty
  if is_empty
    then return []
    else do
      name <- getByteString 16
      when ('/' `CBS.elem` name && CBS.take 3 name /= "#1/") $
        fail "Looks like GNU Archive"
      time <- getPaddedInt <$> getByteString 12
      own <- getPaddedInt <$> getByteString 6
      grp <- getPaddedInt <$> getByteString 6
      mode <- getPaddedInt <$> getByteString 8
      st_size <- getPaddedInt <$> getByteString 10
      end <- getByteString 2
      when (end /= "\x60\x0a") $
        fail
          ( "[BSD Archive] Invalid archive header end marker for name: "
              ++ CBS.unpack name
          )
      off1 <- liftM fromIntegral bytesRead :: Get Int
      -- BSD stores extended filenames, by writing #1/<length> into the
      -- name field, the first @length@ bytes then represent the file name
      -- thus the payload size is filesize + file name length.
      name <-
        if CBS.unpack (CBS.take 3 name) == "#1/"
          then
            liftM
              (CBS.unpack . CBS.takeWhile (/= '\0'))
              (getByteString $ read $ CBS.unpack $ CBS.drop 3 name)
          else return $ CBS.unpack $ CBS.takeWhile (/= ' ') name
      off2 <- liftM fromIntegral bytesRead :: Get Int
      file <- getByteString (st_size - (off2 - off1))
      -- data sections are two byte aligned (see Trac #15396)
      when (odd st_size) $
        void (getByteString 1)
      rest <- getBSDArchEntries
      return $ (GHC.ArchiveEntry name time own grp mode (st_size - (off2 - off1)) file) : rest

-- | GNU Archives feature a special '//' entry that contains the
-- extended names. Those are referred to as /<num>, where num is the
-- offset into the '//' entry.
-- In addition, filenames are terminated with '/' in the archive.
getGNUArchEntries :: Maybe GHC.ArchiveEntry -> Get [GHC.ArchiveEntry]
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
          ( "[BSD Archive] Invalid archive header end marker for name: "
              ++ CBS.unpack name
          )
      file <- getByteString st_size
      -- data sections are two byte aligned (see Trac #15396)
      when (odd st_size) $
        void (getByteString 1)
      name <-
        return . CBS.unpack $
          if CBS.unpack (CBS.take 1 name) == "/"
            then case CBS.takeWhile (/= ' ') name of
              name@"/" -> name -- symbol table
              name@"//" -> name -- extended file names table
              name -> getExtName extInfo (read . CBS.unpack $ CBS.drop 1 name)
            else CBS.takeWhile (/= '/') name
      case name of
        "/" -> getGNUArchEntries extInfo
        "//" -> getGNUArchEntries (Just (GHC.ArchiveEntry name time own grp mode st_size file))
        _ -> (GHC.ArchiveEntry name time own grp mode st_size file :) <$> getGNUArchEntries extInfo
  where
    getExtName :: Maybe GHC.ArchiveEntry -> Int -> BS.ByteString
    getExtName Nothing _ = error "Invalid extended filename reference."
    getExtName (Just info) offset = CBS.takeWhile (/= '/') . CBS.drop offset $ GHC.filedata info

getArchMagic :: Get ()
getArchMagic = do
  magic <- liftM CBS.unpack $ getByteString 8
  if magic /= "!<arch>\n"
    then fail $ "Invalid magic number " ++ show magic
    else return ()

getArch :: Get GHC.Archive
getArch = GHC.Archive <$> do
  getArchMagic
  getBSDArchEntries <|> getGNUArchEntries Nothing

parseAr :: BS.ByteString -> GHC.Archive
parseAr = runGet getArch . LBS.fromChunks . pure

ghcLoadAr :: FilePath -> IO GHC.Archive
ghcLoadAr fp = parseAr <$> BS.readFile fp
