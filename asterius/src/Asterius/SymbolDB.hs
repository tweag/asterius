{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.SymbolDB
  ( chase
  ) where

import Asterius.Internals
import Asterius.Types
import Data.Data (Data)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import GHC.Exts
import GHC.Generics
import Prelude hiding (IO)

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
     AsteriusStore
  -> AsteriusEntitySymbol
  -> (forall t. Data t =>
                  t -> a)
  -> EntitySymbolStatus a
queryAsteriusEntitySymbol AsteriusStore {..} sym f =
  case HM.lookup sym symbolMap of
    Just mod_sym -> do
      let AsteriusModule {..} = moduleMap HM.! mod_sym
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
    _ -> Unfound

collectAsteriusEntitySymbol :: Data a => a -> HS.HashSet AsteriusEntitySymbol
collectAsteriusEntitySymbol = collect proxy#

data ChaseResult = ChaseResult
  { directDepBy :: HM.HashMap AsteriusEntitySymbol (HS.HashSet AsteriusEntitySymbol)
  , statusMap :: HM.HashMap (EntitySymbolStatus ()) (HS.HashSet AsteriusEntitySymbol)
  } deriving (Show)

type ChaseState = (HS.HashSet AsteriusEntitySymbol, ChaseResult)

chaseIter :: AsteriusStore -> ChaseState -> ChaseState
chaseIter c (staging_syms, ChaseResult {..}) =
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
  where
    (unfounds, unavailables, availables) =
      splitEntitySymbolStatus
        (map
           (\staging_sym ->
              ( staging_sym
              , queryAsteriusEntitySymbol
                  c
                  staging_sym
                  collectAsteriusEntitySymbol))
           (HS.toList staging_syms))

chase :: AsteriusStore -> AsteriusEntitySymbol -> ChaseResult
chase c sym =
  snd $
  go
    ( [sym]
    , ChaseResult
        { directDepBy = []
        , statusMap = [(Unfound, []), (Unavailable, []), (Available (), [])]
        })
  where
    go s@(_, r0) =
      if f r0 == f r1
        then t
        else go t
      where
        t@(_, r1) = chaseIter c s
    f ChaseResult {..} = HM.foldl' (+) 0 $ HM.map HS.size statusMap
