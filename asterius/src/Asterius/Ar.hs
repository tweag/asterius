{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

getArchive :: Get [BS.ByteString]
getArchive = getArchMagic *> getMany getEntry

walkArchiveFile :: FilePath -> IO [BS.ByteString]
walkArchiveFile path =
  runGet getArchive . LBS.fromChunks . pure <$> BS.readFile path
