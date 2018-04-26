{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Asterius.SymbolDB where

import Asterius.CodeGen
import Asterius.Internals
import Asterius.Types
import Data.Data (Data)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef
import Data.Serialize
import GHC.Exts
import Prelude hiding (IO)
import System.FilePath

newtype AsteriusSymbolDB = AsteriusSymbolDB
  { unAsteriusSymbolDB :: HM.HashMap AsteriusEntitySymbol AsteriusModuleSymbol
  } deriving (Semigroup, Monoid, IsList, Show, Serialize)

enrichSymbolDB ::
     AsteriusModuleSymbol
  -> AsteriusModule
  -> AsteriusSymbolDB
  -> AsteriusSymbolDB
enrichSymbolDB mod_sym AsteriusModule {..} (AsteriusSymbolDB m) =
  AsteriusSymbolDB $
  HM.unions
    [f staticsMap, f staticsErrorMap, f functionMap, f functionErrorMap, m]
  where
    f = HM.map $ const mod_sym

data AsteriusModuleCache = AsteriusModuleCache
  { asteriusModuleTopDir :: FilePath
  , asteriusSymbolDB :: AsteriusSymbolDB
  , asteriusModuleCacheRef :: IORef (HM.HashMap AsteriusModuleSymbol AsteriusModule)
  }

initAsteriusModuleCache :: FilePath -> IO AsteriusModuleCache
initAsteriusModuleCache obj_topdir = do
  sym_db <- decodeFile $ obj_topdir </> "asterius_sym_db"
  cache_ref <- newIORef mempty
  pure
    AsteriusModuleCache
      { asteriusModuleTopDir = obj_topdir
      , asteriusSymbolDB = sym_db
      , asteriusModuleCacheRef = cache_ref
      }

loadAsteriusModule ::
     AsteriusModuleCache -> AsteriusModuleSymbol -> IO AsteriusModule
loadAsteriusModule AsteriusModuleCache {..} mod_sym = do
  prev_cache <- readIORef asteriusModuleCacheRef
  case HM.lookup mod_sym prev_cache of
    Just m -> pure m
    _ -> do
      p <- moduleSymbolPath asteriusModuleTopDir mod_sym "asterius_o"
      m <- decodeFile p
      atomicModifyIORef' asteriusModuleCacheRef $ \cache ->
        (HM.insert mod_sym m cache, ())
      pure m

enrichAsteriusModuleCache ::
     AsteriusModuleSymbol
  -> AsteriusModule
  -> AsteriusModuleCache
  -> IO AsteriusModuleCache
enrichAsteriusModuleCache mod_sym m c@AsteriusModuleCache {..} = do
  atomicModifyIORef' asteriusModuleCacheRef $ \cache ->
    (HM.insert mod_sym m cache, ())
  pure c {asteriusSymbolDB = enrichSymbolDB mod_sym m asteriusSymbolDB}

data EntitySymbolStatus
  = Unfound
  | Unavailable
  | Available

availAsteriusEntitySymbol ::
     AsteriusModuleCache
  -> AsteriusModuleSymbol
  -> AsteriusEntitySymbol
  -> IO EntitySymbolStatus
availAsteriusEntitySymbol c mod_sym sym = do
  AsteriusModule {..} <- loadAsteriusModule c mod_sym
  pure $
    case entityKind sym of
      StaticsEntity
        | HM.member sym staticsMap -> Available
        | HM.member sym staticsErrorMap -> Unavailable
        | otherwise -> Unfound
      FunctionEntity
        | HM.member sym functionMap -> Available
        | HM.member sym functionErrorMap -> Unavailable
        | otherwise -> Unfound

collectAsteriusEntitySymbol :: Data a => a -> HS.HashSet AsteriusEntitySymbol
collectAsteriusEntitySymbol = collect proxy#

data ChaseError = ChaseError
  { unfoundBy, unavailableBy :: HM.HashMap AsteriusEntitySymbol (HS.HashSet AsteriusEntitySymbol)
  } deriving (Show)
