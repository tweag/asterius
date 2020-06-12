{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Asterius.Types.DependencyMap
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- TODO: Explain.
module Asterius.Types.DependencyMap
  ( DependencyMap,
    toDependencyMap,
    lookup,
    (!),
  )
where

import Asterius.Types.EntitySymbol
import Asterius.Types.SymbolMap (SymbolMap)
import qualified Asterius.Types.SymbolMap as SM
import Asterius.Types.SymbolSet (SymbolSet)
import Binary
import Control.DeepSeq
import Data.Coerce
import Data.Data
import GHC.Stack
import Prelude hiding (lookup)

newtype DependencyMap = DependencyMap (SymbolMap SymbolSet)
  deriving newtype (Eq, Semigroup, Monoid, NFData)
  deriving stock (Data)

instance Binary DependencyMap where
  put_ bh (DependencyMap m) = put_ bh m -- TODO: lazyPut bh m
  get bh = DependencyMap <$> get bh -- TODO: lazyGet bh

instance Show DependencyMap where
  showsPrec = coerce (showsPrec @(SymbolMap SymbolSet))

-- ----------------------------------------------------------------------------

{-# INLINE toDependencyMap #-}
toDependencyMap :: SymbolMap SymbolSet -> DependencyMap
toDependencyMap = coerce

-- | /O(min(n,W))/. Lookup the value at an 'EntitySymbol' in the map.
lookup :: EntitySymbol -> DependencyMap -> Maybe SymbolSet
lookup = coerce (SM.lookup @SymbolSet)

-- | /O(min(n,W))/. Find the value at an 'EntitySymbol'. Calls 'error' when the
-- element can not be found.
{-# INLINE (!) #-}
(!) :: HasCallStack => DependencyMap -> EntitySymbol -> SymbolSet
(!) = coerce ((SM.!) @SymbolSet)

infixl 9 !
