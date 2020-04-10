{-# OPTIONS_GHC -Wno-orphans #-}

module Asterius.Binary.Orphans
  (
  )
where

import qualified Binary as GHC
import Control.Monad
import Data.Foldable
import qualified Data.Map.Lazy as M
import GHC.Float

instance GHC.Binary Float where
  put_ bh = GHC.put_ bh . castFloatToWord32
  get bh = castWord32ToFloat <$> GHC.get bh

instance GHC.Binary Double where
  put_ bh = GHC.put_ bh . castDoubleToWord64
  get bh = castWord64ToDouble <$> GHC.get bh

instance (GHC.Binary k, GHC.Binary v) => GHC.Binary (M.Map k v) where
  put_ bh m =
    GHC.put_ bh (M.size m)
      *> for_ (M.toAscList m) (\(k, v) -> GHC.put_ bh k *> GHC.lazyPut bh v)
  get bh =
    fmap M.fromDistinctAscList $
      GHC.get bh
        >>= flip
          replicateM
          ((,) <$> GHC.get bh <*> GHC.lazyGet bh)
