{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Asterius.Ar
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Loading of @ar@ files from disk. We use a GNU-style format for archives; the
-- first entry is always the extended filename table. In the second entry we
-- include the dependency map created by combining all the dependency maps from
-- the object files that follow.
module Asterius.Ar
  ( loadAr,
    arIndexFileName,
    createArchive,
  )
where

import qualified Ar as GHC
import Asterius.Binary.ByteString
import Asterius.Binary.NameCache
import Asterius.Types
import Asterius.Types.SymbolMap (SymbolMap)
import Asterius.Types.SymbolSet (SymbolSet)
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Traversable
import GHC.IO.Unsafe
import qualified IfaceEnv as GHC
import System.FilePath

createIndex :: [BS.ByteString] -> IO (SymbolMap SymbolSet)
createIndex blobs = do
  ncu <- newNameCacheUpdater
  mconcat <$> for blobs (getBS ncu) -- Get the dependencyMap only.

createIndexEntry :: [BS.ByteString] -> IO GHC.ArchiveEntry
createIndexEntry blobs = do
  index <- createIndex blobs
  blob <- putBS index
  pure
    GHC.ArchiveEntry
      { GHC.filename = arIndexFileName,
        GHC.filetime = 0,
        GHC.fileown = 0,
        GHC.filegrp = 0,
        GHC.filemode = 0o644,
        GHC.filesize = BS.length blob,
        GHC.filedata = blob
      }

-- | Create a library archive from a bunch of object files, using @Ar@ from the
-- GHC API. Though the name of each object file is preserved, we set the
-- timestamp, owner ID, group ID, and file mode to default values (0, 0, 0, and
-- 0644, respectively). When we deserialize (see @Asterius.Ar.loadAr@), the
-- metadata is ignored anyway.
createArchive :: FilePath -> [FilePath] -> IO ()
createArchive arFile objFiles = do
  blobs <- for objFiles (unsafeDupableInterleaveIO . BS.readFile)
  index <- createIndexEntry blobs
  GHC.writeGNUAr arFile
    $ GHC.Archive
    $ index
      : [ GHC.ArchiveEntry
            { GHC.filename = takeFileName obj_path,
              GHC.filetime = 0,
              GHC.fileown = 0,
              GHC.filegrp = 0,
              GHC.filemode = 0o644,
              GHC.filesize = BS.length blob,
              GHC.filedata = blob
            }
          | (obj_path, blob) <- zip objFiles blobs
        ]

arIndexFileName :: FilePath
arIndexFileName = ".asterius.index.bin"

-- | Load the contents of an archive (@.a@) file as an 'AsteriusCachedModule'.
-- 'loadAr' ignores (@.o@) files in the archive that cannot be parsed. Also,
-- the metadata of the contained files are ignored (@ahc-ar@ always sets them
-- to default values anyway). If the metadata are really needed, make sure to
-- update @ahc-ar@ to generate non-default values for them.
loadAr :: GHC.NameCacheUpdater -> FilePath -> IO AsteriusCachedModule
loadAr ncu p = do
  GHC.Archive entries <- asteriusLoadAr p
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

getAllArEntryFields :: Get (CBS.ByteString, Int, Int, Int, Int, Int, CBS.ByteString)
getAllArEntryFields = do
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
  return (name, time, own, grp, mode, st_size, file)

getExtendedFileNamesTable :: Get BS.ByteString
getExtendedFileNamesTable = do
  (name, _time, _own, _grp, _mode, _st_size, file) <- getAllArEntryFields
  if CBS.takeWhile (/= ' ') name == "//"
    then pure file
    else fail "[GNU Archive] First entry must be the filename table."

getArchiveIndex :: CBS.ByteString -> Get BS.ByteString -- (SymbolMap SymbolSet)
getArchiveIndex filenamesTable = do
  entry <- getGNUArchEntry filenamesTable
  if GHC.filename entry == arIndexFileName
    then pure $ GHC.filedata entry
    else fail "[GNU Archive] Second entry must be the index."

-- getMany :: Get a -> Get [a]
-- getMany fn = do
--   is_empty <- isEmpty
--   if is_empty
--     then return []
--     else (:) <$> fn <*> getMany fn

foldlGNUArchEntries :: (b -> GHC.ArchiveEntry -> b) -> b -> Get b
foldlGNUArchEntries f z = do
  filenamesTable <- getExtendedFileNamesTable
  _ <- getArchiveIndex filenamesTable
  go filenamesTable z
  where
    go table !acc = do
      is_empty <- isEmpty
      if is_empty
        then return acc
        else do
          entry <- getGNUArchEntry table
          go table (f acc entry)

-- | GNU Archives feature a special '//' entry that contains the
-- extended names. Those are referred to as /<num>, where num is the
-- offset into the '//' entry.
-- In addition, filenames are terminated with '/' in the archive.
-- @putGNUArch@ always places the filename table first.
getGNUArchEntries :: Get [GHC.ArchiveEntry]
getGNUArchEntries = do
  -- filenamesTable <- getExtendedFileNamesTable
  -- _ <- getArchiveIndex filenamesTable
  -- getMany (getGNUArchEntry filenamesTable)
  foldlGNUArchEntries (flip (:)) [] -- reverse order

-- | Get a GNU-style archive entry (NOT the filename table entry).
getGNUArchEntry :: CBS.ByteString -> Get GHC.ArchiveEntry
getGNUArchEntry filenamesTable = do
  (name, time, own, grp, mode, st_size, file) <- getAllArEntryFields
  let real_name = getRealFileName filenamesTable name
  pure $ GHC.ArchiveEntry real_name time own grp mode st_size file

getRealFileName :: CBS.ByteString -> CBS.ByteString -> String
getRealFileName filenamesTable name =
  CBS.unpack $
    if CBS.unpack (CBS.take 1 name) == "/"
      then
        getExtName filenamesTable
          $ read
          $ CBS.unpack
          $ CBS.drop 1
          $ CBS.takeWhile (/= ' ') name
      else CBS.takeWhile (/= '/') name
  where
    getExtName :: BS.ByteString -> Int -> BS.ByteString
    getExtName info offset = CBS.takeWhile (/= '/') $ CBS.drop offset info

getArchMagic :: Get ()
getArchMagic = do
  magic <- liftM CBS.unpack $ getByteString 8
  when (magic /= "!<arch>\n")
    $ fail
    $ "Invalid magic number " ++ show magic

getArch :: Get GHC.Archive
getArch = GHC.Archive <$> do
  getArchMagic
  getGNUArchEntries

parseAr :: BS.ByteString -> GHC.Archive
parseAr = runGet getArch . LBS.fromChunks . pure

asteriusLoadAr :: FilePath -> IO GHC.Archive
asteriusLoadAr fp = parseAr <$> BS.readFile fp
