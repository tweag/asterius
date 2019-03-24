{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Ar
  ( loadAr
  ) where

import qualified Ar as GHC
import Asterius.Internals
import Asterius.Internals.Binary
import Asterius.Types
import Control.Exception
import Data.List
import Prelude hiding (IO)

loadAr :: FilePath -> IO AsteriusModule
loadAr p = do
  GHC.Archive entries <- GHC.loadAr p
  evaluate $
    foldl'
      (\acc GHC.ArchiveEntry {..} ->
         case decodeMaybe filedata of
           Just m -> m <> acc
           _ -> acc)
      mempty
      entries
