{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Ar
  ( loadAr,
  )
where

import qualified Ar as GHC
import Asterius.Binary.ByteString
import Asterius.Types
import Data.Foldable
import qualified IfaceEnv as GHC

-- | Load the contents of an archive (@.a@) file as an 'AsteriusCachedModule'.
-- 'loadAr' ignores (@.o@) files in the archive that cannot be parsed. Also,
-- the metadata of the contained files are ignored (@ahc-ar@ always sets them
-- to default values anyway). If the metadata are really needed, make sure to
-- update @ahc-ar@ to generate non-default values for them.
loadAr :: GHC.NameCacheUpdater -> FilePath -> IO AsteriusCachedModule
loadAr ncu p = do
  GHC.Archive entries <- GHC.loadAr p
  foldlM
    ( \acc GHC.ArchiveEntry {..} -> tryGetBS ncu filedata >>= \case
        Left _ -> pure acc
        Right m -> pure $ m <> acc
    )
    mempty
    entries
