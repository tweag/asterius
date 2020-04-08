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

loadAr :: FilePath -> IO AsteriusModule
loadAr p = do
  GHC.Archive entries <- GHC.loadAr p
  foldlM
    ( \acc GHC.ArchiveEntry {..} -> tryGetBS filedata >>= \case
        Left _ -> pure acc
        Right m -> pure $ m <> acc
    )
    mempty
    entries
