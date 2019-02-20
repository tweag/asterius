{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Ar
  ( loadAr
  ) where

import qualified Ar as GHC
import Asterius.Internals
import Asterius.Types
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IMap
import Data.List
import qualified Data.Map.Lazy as LMap
import Data.Map.Lazy (Map)
import Prelude hiding (IO)

loadAr :: FilePath -> IO AsteriusStore
loadAr p = do
  GHC.Archive entries <- GHC.loadAr p
  let files_map = IMap.fromList $ zip [0 ..] $ map GHC.filedata entries
      (mod_sym_map :: Map AsteriusModuleSymbol Int) =
        IMap.foldlWithKey'
          (\tot i buf ->
             case decodeOrFail (LBS.fromStrict buf) of
               Left _ -> tot
               Right (_, _, mod_sym) -> LMap.insert mod_sym i tot)
          LMap.empty
          files_map
      Just index_entry = find ((== "INDEX") . GHC.filename) entries
      (sym_map :: Map AsteriusEntitySymbol AsteriusModuleSymbol) =
        decode $ LBS.fromStrict $ GHC.filedata index_entry
      mod_map =
        LMap.map (decode . LBS.fromStrict . (files_map IMap.!)) mod_sym_map
  pure AsteriusStore {symbolMap = sym_map, moduleMap = mod_map}
