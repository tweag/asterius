{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Internals.SYB
  ( GenericM,
    everywhereM,
    GenericQ,
    everything,
  )
where

import Data.Data
  ( Data,
    gmapM,
    gmapQl,
  )

type GenericM m = forall a. Data a => a -> m a

{-# INLINE everywhereM #-}
everywhereM :: forall m. Monad m => GenericM m -> GenericM m
everywhereM f = w
  where
    w :: GenericM m
    w = (>>= gmapM w) . f

type GenericQ r = forall a. Data a => a -> r

{-# INLINEABLE everything #-}
everything :: forall r. Monoid r => GenericQ r -> GenericQ r
everything f = w
  where
    w :: GenericQ r
    w x = gmapQl (<>) (f x) w x
