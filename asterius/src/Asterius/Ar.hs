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
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Foldable
import Data.Traversable
import GHC.IO.Unsafe
import qualified IfaceEnv as GHC
import System.Exit (die)
import System.IO

-------------------------------------------------------------------------------

newtype Archive = Archive [BS.ByteString]
  deriving (Eq, Show, Semigroup, Monoid)

-------------------------------------------------------------------------------

putPaddedInt :: Int -> Int -> Put
putPaddedInt padding i = putPaddedString '\x20' padding (show i)

putPaddedString :: Char -> Int -> String -> Put
putPaddedString pad padding s =
  putByteString . CBS.pack . take padding $ s <> repeat pad

putArchMagic :: Put
putArchMagic = putByteString $ CBS.pack "!<arch>\n"

-- | Put an archive entry. Note that this assumes that the entry has been
-- preprocessed to account for the extended file name table section. That is,
-- the name is assumed to fit in 16 characters; if it's longer, it gets
-- truncated.
putArchEntry :: BS.ByteString -> PutM ()
putArchEntry file_contents = do
  -- total: 60 bytes
  putByteString "                " -- name, 16 bytes
  putByteString "0           " -- mtime, 12 bytes
  putByteString "0     " -- UID, 6 bytes
  putByteString "0     " -- GID, 6 bytes
  putByteString "0644    " -- mode, 8 bytes
  putPaddedInt 10 st_size -- file length, 10 bytes
  putByteString "\x60\x0a" -- header magic, 2 bytes
  putByteString file_contents
  when (st_size `mod` 2 == 1) $
    putWord8 0x0a
  where
    st_size = BS.length file_contents

putArchive :: Archive -> PutM ()
putArchive (Archive as) = putArchMagic >> mapM_ putArchEntry as

-- | Create a library archive from a bunch of object files. Though the name of
-- each object file is preserved, we set the timestamp, owner ID, group ID, and
-- file mode to default values (0, 0, 0, and 0644, respectively). When we
-- deserialize (see 'loadArchiveRep'), the metadata is ignored anyway.
createArchive :: FilePath -> [FilePath] -> IO ()
createArchive arFile objFiles = do
  blobs <- for objFiles (unsafeDupableInterleaveIO . BS.readFile)
  writeArchiveToFile arFile $ Archive blobs

writeArchiveToFile :: FilePath -> Archive -> IO ()
writeArchiveToFile fp = LBS.writeFile fp . runPut . putArchive

-------------------------------------------------------------------------------

-- | Load the contents of an archive (@.a@) file. 'loadArchive' ignores (@.o@)
-- files in the archive that cannot be parsed. Also, the metadata of the
-- contained files are ignored ('createArchive' always sets them to default
-- values anyway).
loadArchive :: GHC.NameCacheUpdater -> FilePath -> IO AsteriusCachedModule
loadArchive ncu p = do
  entries <- walkArchiveFile p
  foldlM
    ( \acc entry -> tryGetBS ncu entry >>= \case
        Left _ -> pure acc
        Right m -> pure $ m <> acc
    )
    mempty
    entries

-------------------------------------------------------------------------------

walkArchiveFile :: FilePath -> IO [CBS.ByteString]
walkArchiveFile path = withBinaryFile path ReadMode $ \h ->
  hFileSize h >>= walkArchive h
  where
    walkError msg = die $ "Asterius.Ar.walkArchiveFile: " ++ msg
    archLF = "!<arch>\x0a" -- global magic, 8 bytes
    walkArchive :: Handle -> Integer -> IO [CBS.ByteString]
    walkArchive h archiveSize = do
      global <- BS.hGet h (BS.length archLF)
      unless (global == archLF) $ walkError "Bad global header"
      go $ toInteger $ BS.length archLF
      where
        go :: Integer -> IO [CBS.ByteString]
        go offset = case compare offset archiveSize of
          EQ -> pure []
          GT -> walkError $ "Archive truncated at offset " ++ show offset
          LT -> do
            -- Get the size of the file (ignore the rest of the header).
            hSeek h AbsoluteSeek (offset + 48)
            objSize <- do
              size <- BS.hGet h 10
              case reads (CBS.unpack size) of
                [(n, s)] | all isSpace s -> pure n
                _ -> walkError $ "Bad file size in header at offset " ++ show offset
            -- Get the contents of the object file.
            hSeek h AbsoluteSeek (offset + 60) -- skip the whole header
            object <- BS.hGet h (fromIntegral objSize)
            -- Get the rest of the contents.
            let nextHeader =
                  offset + 60 + if odd objSize then objSize + 1 else objSize
            hSeek h AbsoluteSeek nextHeader
            objects <- go nextHeader
            -- Combine em.
            pure (object : objects)
