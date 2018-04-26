{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
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
import Data.Hashable
import Data.IORef
import Data.Serialize
import Data.Traversable
import GHC.Exts
import GHC.Generics
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

data EntitySymbolStatus a
  = Unfound
  | Unavailable
  | Available a
  deriving (Eq, Show, Generic, Functor)

instance Hashable a => Hashable (EntitySymbolStatus a)

splitEntitySymbolStatus :: [(k, EntitySymbolStatus a)] -> ([k], [k], [(k, a)])
splitEntitySymbolStatus =
  foldr
    (\(k, st) (unfounds, unavailables, availables) ->
       case st of
         Unfound -> (k : unfounds, unavailables, availables)
         Unavailable -> (unfounds, k : unavailables, availables)
         Available a -> (unfounds, unavailables, (k, a) : availables))
    ([], [], [])

queryAsteriusEntitySymbol ::
     AsteriusModuleCache
  -> AsteriusEntitySymbol
  -> (forall t. Data t =>
                  t -> a)
  -> IO (EntitySymbolStatus a)
queryAsteriusEntitySymbol c@AsteriusModuleCache {..} sym f =
  case HM.lookup sym $ unAsteriusSymbolDB asteriusSymbolDB of
    Just mod_sym -> do
      AsteriusModule {..} <- loadAsteriusModule c mod_sym
      pure $
        case entityKind sym of
          StaticsEntity ->
            case HM.lookup sym staticsMap of
              Just ss -> Available $ f ss
              _
                | HM.member sym staticsErrorMap -> Unavailable
                | otherwise -> Unfound
          FunctionEntity ->
            case HM.lookup sym functionMap of
              Just func -> Available $ f func
              _
                | HM.member sym functionErrorMap -> Unavailable
                | otherwise -> Unfound
    _ -> pure Unfound

collectAsteriusEntitySymbol :: Data a => a -> HS.HashSet AsteriusEntitySymbol
collectAsteriusEntitySymbol = collect proxy#

data ChaseResult = ChaseResult
  { directDepBy :: HM.HashMap AsteriusEntitySymbol (HS.HashSet AsteriusEntitySymbol)
  , statusMap :: HM.HashMap (EntitySymbolStatus ()) (HS.HashSet AsteriusEntitySymbol)
  } deriving (Show)

type ChaseState = (HS.HashSet AsteriusEntitySymbol, ChaseResult)

chaseIter :: AsteriusModuleCache -> ChaseState -> IO ChaseState
chaseIter c (staging_syms, ChaseResult {..}) = do
  r <-
    for (HS.toList staging_syms) $ \staging_sym -> do
      r <- queryAsteriusEntitySymbol c staging_sym collectAsteriusEntitySymbol
      pure (staging_sym, r)
  let (unfounds, unavailables, availables) = splitEntitySymbolStatus r
  pure
    ( HS.filter (\sym -> not $ any (sym `HS.member`) statusMap) $
      HS.unions $ map snd availables
    , ChaseResult
        { directDepBy =
            foldr
              (\(k, k0) ->
                 HM.alter
                   (\case
                      Just k0s -> Just $ HS.insert k0 k0s
                      _ -> Just $ HS.singleton k0)
                   k)
              directDepBy $
            concat [[(k1, k) | k1 <- HS.toList k1s] | (k, k1s) <- availables]
        , statusMap =
            HM.unionWith
              (<>)
              [ (Unfound, HS.fromList unfounds)
              , (Unavailable, HS.fromList unavailables)
              , (Available (), HS.fromList $ map fst availables)
              ]
              statusMap
        })

chase :: AsteriusModuleCache -> AsteriusEntitySymbol -> IO ChaseResult
chase c sym =
  snd <$>
  go
    ( [sym]
    , ChaseResult
        { directDepBy = []
        , statusMap = [(Unfound, []), (Unavailable, []), (Available (), [])]
        })
  where
    go s@(_, r0) = do
      t@(_, r1) <- chaseIter c s
      if f r0 == f r1
        then pure t
        else go t
    f ChaseResult {..} = HM.foldl' (+) 0 $ HM.map HS.size statusMap
