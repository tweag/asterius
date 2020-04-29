{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Ar
  ( loadAr,
    loadArchiveEntries,
    loadArchiveEntry
  )
where

import qualified Ar as GHC
import Asterius.Binary.ByteString
import Asterius.Types
import qualified IfaceEnv as GHC

-- | Load an archive file from disk, deserialize all objects it contains and
-- concatenate them into a single 'AsteriusCachedModule'.
loadAr :: GHC.NameCacheUpdater -> FilePath -> IO AsteriusCachedModule
loadAr ncu p = do -- It is still being used by Asterius.GHCi.Internals.asteriusIservCall
  entries <- loadArchiveEntries p
  mconcat <$> mapM (loadArchiveEntry ncu) entries

-- | Load all the archive entries from an archive file @.a@, as a list of plain
-- 'ByteString's (content only).
{-# INLINE loadArchiveEntries #-}
loadArchiveEntries :: FilePath -> IO [GHC.ArchiveEntry]
loadArchiveEntries p = do -- drop ncu
  GHC.Archive entries <- GHC.loadAr p
  return entries

-- | Deserialize an 'GHC.ArchiveEntry'. In case deserialization fails, return
-- an empty 'AsteriusModule'.
loadArchiveEntry :: GHC.NameCacheUpdater -> GHC.ArchiveEntry -> IO AsteriusCachedModule
loadArchiveEntry ncu = \GHC.ArchiveEntry {..} ->
  tryGetBS ncu filedata >>= \case
    Left {} -> pure mempty
    Right m -> pure m
