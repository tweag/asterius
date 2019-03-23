{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Ar
  ( loadAr
  ) where

import qualified Ar as GHC
import Asterius.Internals
import Asterius.Types
import Control.Exception
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Prelude hiding (IO)

loadAr :: FilePath -> IO AsteriusModule
loadAr p = do
  GHC.Archive entries <- GHC.loadAr p
  evaluate $
    foldl'
      (\acc GHC.ArchiveEntry {..} ->
         case decodeOrFail $ LBS.fromStrict filedata of
           Left _ -> acc
           Right (_, _, m) -> m <> acc)
      mempty
      entries
