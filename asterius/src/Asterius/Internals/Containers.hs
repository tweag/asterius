module Asterius.Internals.Containers
  ( sortKeysByIntValue
  ) where

import qualified Data.IntMap.Strict as IMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

{-# INLINE sortKeysByIntValue #-}
sortKeysByIntValue :: Map k Int -> [k]
sortKeysByIntValue =
  IMap.elems . Map.foldlWithKey' (\tot k v -> IMap.insert v k tot) IMap.empty
