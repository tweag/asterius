{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Ar
  ( loadAr
  ) where

import qualified Ar as GHC
import Asterius.Internals
import Asterius.Types
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Lazy as LMap
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Prelude hiding (IO)

loadAr :: FilePath -> IO AsteriusStore
loadAr p = do
  GHC.Archive entries <- GHC.loadAr p
  let files_map =
        Map.fromList [(filename, filedata) | GHC.ArchiveEntry {..} <- entries]
      (mod_sym_map :: Map AsteriusModuleSymbol FilePath) =
        Map.foldlWithKey'
          (\tot obj_path buf ->
             case decodeOrFail (LBS.fromStrict buf) of
               Left _ -> tot
               Right (_, _, mod_sym) -> Map.insert mod_sym obj_path tot)
          Map.empty
          files_map
      (sym_map :: Map AsteriusEntitySymbol AsteriusModuleSymbol) =
        decode $ LBS.fromStrict $ files_map ! "INDEX"
      mod_map = LMap.map (decode . LBS.fromStrict . (files_map !)) mod_sym_map
  pure AsteriusStore {symbolMap = sym_map, moduleMap = mod_map}
