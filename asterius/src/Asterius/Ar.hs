{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Ar
  ( loadAr,
  )
where

import qualified Ar as GHC
import Asterius.Binary.ByteString
import Asterius.Internals.Parallel
import Asterius.Types
import qualified IfaceEnv as GHC

loadAr :: GHC.NameCacheUpdater -> FilePath -> IO AsteriusCachedModule
loadAr ncu p = do
  GHC.Archive entries <- GHC.loadAr p
  parallelFor 1 (reverse entries) $ \GHC.ArchiveEntry {..} -> -- TODO: Parameterize
    tryGetBS ncu filedata >>= \case
      Left {} -> pure mempty
      Right m -> pure m
