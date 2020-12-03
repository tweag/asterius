{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Asterius.Ar
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Loading of @ar@ files.
module Asterius.Ar
  ( loadArchive,
  )
where

import qualified Ar as GHC
import Asterius.Binary.ByteString
import Asterius.Types
import qualified Data.ByteString as BS
import Data.Foldable
import qualified IfaceEnv as GHC
import System.Directory

-------------------------------------------------------------------------------

-- | Load the contents of an archive (@.a@) file. 'loadArchive' ignores (@.o@)
-- files in the archive that cannot be parsed. Also, the metadata of the
-- contained files are ignored.
loadArchive :: GHC.NameCacheUpdater -> FilePath -> IO AsteriusCachedModule
loadArchive ncu p = do
  entries <- walkArchiveFile p
  foldlM
    ( \acc entry ->
        tryGetBS ncu entry >>= \case
          Left _ -> pure acc
          Right m -> pure $ m <> acc
    )
    mempty
    entries

walkArchiveFile :: FilePath -> IO [BS.ByteString]
walkArchiveFile path' = do
  path <- makeAbsolute path'
  GHC.Archive entries <- GHC.loadAr path
  pure $ map GHC.filedata entries
