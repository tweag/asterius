{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Asterius.Ar
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Creation and loading of @ar@ files. We use a GNU-style format for archives;
-- the first entry is always the extended filename table, named @"//"@, and the
-- following entries are the object files.
--
-- Historical Note: This implementation is adapted from GHC's Ar.hs. Initially,
-- ahc-cabal used GNU ar internally, but that was non portable (see issues #649
-- and #345). Hence, we implemented @ahc-ar@ to better control how archive
-- files are created, and committed to GNU-style archives. Since we have
-- further plans of customizing the treatment of archive files, we chose to
-- roll out our own implementation of @loadArchive@/@createArchive@ (based on
-- that of GHC).
module Asterius.Ar
  ( loadArchive,
    createArchive,
  )
where

import Asterius.Binary.ByteString
import Asterius.Types
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Traversable
import GHC.IO.Unsafe
import qualified IfaceEnv as GHC
import System.FilePath

-------------------------------------------------------------------------------

data ArchiveEntry
  = ArchiveEntry
      { -- | File name.
        filename :: String,
        -- | File modification time.
        filetime :: Int,
        -- | File owner.
        fileown :: Int,
        -- | File group.
        filegrp :: Int,
        -- | File mode.
        filemode :: Int,
        -- | File size.
        filesize :: Int,
        -- | File bytes.
        filedata :: BS.ByteString
      }
  deriving (Eq, Show)

newtype Archive = Archive [ArchiveEntry]
  deriving (Eq, Show, Semigroup, Monoid)

-------------------------------------------------------------------------------

-- | Archives have numeric values padded with '\x20' to the right.
getPaddedInt :: BS.ByteString -> Int
getPaddedInt = read . CBS.unpack . CBS.takeWhile (/= '\x20')

putPaddedInt :: Int -> Int -> Put
putPaddedInt padding i = putPaddedString '\x20' padding (show i)

putPaddedString :: Char -> Int -> String -> Put
putPaddedString pad padding s =
  putByteString . CBS.pack . take padding $ s <> repeat pad

putArchMagic :: Put
putArchMagic = putByteString $ CBS.pack "!<arch>\n"

getArchMagic :: Get ()
getArchMagic = do
  magic <- liftM CBS.unpack $ getByteString 8
  when (magic /= "!<arch>\n")
    $ fail
    $ "Invalid magic number " ++ show magic

-- | put an Archive Entry. This assumes that the entries
-- have been preprocessed to account for the extenden file name
-- table section "//" e.g. for GNU Archives. Or that the names
-- have been move into the payload for BSD Archives.
putArchEntry :: ArchiveEntry -> PutM ()
putArchEntry (ArchiveEntry name time own grp mode st_size file) = do
  putPaddedString ' ' 16 name
  putPaddedInt 12 time
  putPaddedInt 6 own
  putPaddedInt 6 grp
  putPaddedInt 8 mode
  putPaddedInt 10 (st_size + pad)
  putByteString "\x60\x0a"
  putByteString file
  when (pad == 1) $
    putWord8 0x0a
  where
    pad = st_size `mod` 2

putGNUArch :: Archive -> PutM ()
putGNUArch (Archive as) = do
  putArchMagic
  mapM_ putArchEntry (processEntries as)
  where
    processEntry :: ArchiveEntry -> ArchiveEntry -> (ArchiveEntry, ArchiveEntry)
    processEntry extInfo archive@(ArchiveEntry name _ _ _ _ _ _)
      | length name > 15 =
        ( extInfo
            { filesize = filesize extInfo + length name + 2,
              filedata = filedata extInfo <> CBS.pack name <> "/\n"
            },
          archive {filename = "/" <> show (filesize extInfo)}
        )
      | otherwise = (extInfo, archive {filename = name <> "/"})
    processEntries :: [ArchiveEntry] -> [ArchiveEntry]
    processEntries =
      uncurry (:) . mapAccumL processEntry (ArchiveEntry "//" 0 0 0 0 0 mempty)

-- | Create a library archive from a bunch of object files.  Though the name of
-- each object file is preserved, we set the timestamp, owner ID, group ID, and
-- file mode to default values (0, 0, 0, and 0644, respectively). When we
-- deserialize (see 'loadArchive'), the metadata is ignored anyway.
createArchive :: FilePath -> [FilePath] -> IO ()
createArchive arFile objFiles = do
  blobs <- for objFiles (unsafeDupableInterleaveIO . BS.readFile)
  writeGNUAr arFile $
    Archive
      [ ArchiveEntry
          { filename = takeFileName obj_path,
            filetime = 0,
            fileown = 0,
            filegrp = 0,
            filemode = 0o644,
            filesize = BS.length blob,
            filedata = blob
          }
        | (obj_path, blob) <- zip objFiles blobs
      ]

writeGNUAr :: FilePath -> Archive -> IO ()
writeGNUAr fp = LBS.writeFile fp . runPut . putGNUArch

-------------------------------------------------------------------------------

-- | Load the contents of an archive (@.a@) file as an 'AsteriusCachedModule'.
-- 'loadArchive' ignores (@.o@) files in the archive that cannot be parsed. Also,
-- the metadata of the contained files are ignored (@ahc-ar@ always sets them
-- to default values anyway). If the metadata are really needed, make sure to
-- update @ahc-ar@ to generate non-default values for them.
loadArchive :: GHC.NameCacheUpdater -> FilePath -> IO AsteriusCachedModule
loadArchive ncu p = do
  Archive entries <- parseAr <$> BS.readFile p
  foldlM
    ( \acc ArchiveEntry {..} -> tryGetBS ncu filedata >>= \case
        Left _ -> pure acc
        Right m -> pure $ m <> acc
    )
    mempty
    entries

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

getMany :: Get a -> Get [a]
getMany fn = do
  is_empty <- isEmpty
  if is_empty
    then return []
    else (:) <$> fn <*> getMany fn

-- | GNU Archives feature a special '//' entry that contains the
-- extended names. Those are referred to as /<num>, where num is the
-- offset into the '//' entry.
-- In addition, filenames are terminated with '/' in the archive.
-- @putGNUArch@ always places the filename table first.
getGNUArchEntries :: Get [ArchiveEntry]
getGNUArchEntries = do
  filenamesTable <- getExtendedFileNamesTable
  getMany (getGNUArchEntry filenamesTable)

-- | Get a GNU-style archive entry (NOT the filename table entry).
getGNUArchEntry :: CBS.ByteString -> Get ArchiveEntry
getGNUArchEntry filenamesTable = do
  (name, time, own, grp, mode, st_size, file) <- getAllArEntryFields
  let real_name = getRealFileName filenamesTable name
  pure $ ArchiveEntry real_name time own grp mode st_size file

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

getArch :: Get Archive
getArch = Archive <$> do
  getArchMagic
  getGNUArchEntries

parseAr :: BS.ByteString -> Archive
parseAr = runGet getArch . LBS.fromChunks . pure
