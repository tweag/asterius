{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Internals.SYB
  ( GenericM
  , everywhereM
  ) where

import Data.Data (Data, gmapM)

type GenericM m
   = forall a. Data a =>
                 a -> m a

{-# INLINE everywhereM #-}
everywhereM ::
     forall m. Monad m
  => GenericM m
  -> GenericM m
everywhereM f = w
  where
    w :: GenericM m
    w = (>>= gmapM w) . f
