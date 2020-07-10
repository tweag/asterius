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
-- Creation and loading of @ar@ files.
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

-- | Put the contents of an object file as an archive entry. Note that this
-- sets all entry metadata, including the filename, to defaults. The only
-- exception to this rule is the filesize which is set to the size of the given
-- contents.
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

-- | Create a library archive from a bunch of object files. All metadata
-- (including the filename) are set to defaults; when we deserialize (see
-- 'loadArchive'), the metadata is ignored anyway.
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
  Archive entries <- walkArchiveFile p
  foldlM
    ( \acc entry -> tryGetBS ncu entry >>= \case
        Left _ -> pure acc
        Right m -> pure $ m <> acc
    )
    mempty
    entries

-- | Archives have numeric values padded with '\x20' to the right.
getPaddedInt :: BS.ByteString -> Int
getPaddedInt = read . CBS.unpack . CBS.takeWhile (/= '\x20')

getArchMagic :: Get ()
getArchMagic = do
  magic <- liftM CBS.unpack $ getByteString 8
  when (magic /= "!<arch>\n")
    $ fail
    $ "Invalid magic number " ++ show magic

getEntry :: Get CBS.ByteString
getEntry = do
  skip 48
  size <- getPaddedInt <$> getByteString 10
  skip 2
  file <- getByteString size
  -- data sections are two byte aligned
  when (odd size) $ skip 1
  pure file

getMany :: Get a -> Get [a]
getMany fn = do
  is_empty <- isEmpty
  if is_empty
    then return []
    else (:) <$> fn <*> getMany fn

getArchive :: Get Archive
getArchive = getArchMagic *> (Archive <$> getMany getEntry)

walkArchiveFile :: FilePath -> IO Archive
walkArchiveFile path =
  runGet getArchive . LBS.fromChunks . pure <$> BS.readFile path
