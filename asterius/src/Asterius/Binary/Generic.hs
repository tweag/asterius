{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Asterius.Binary.Generic
  ( gPut_,
    gGet,
  )
where

import qualified Binary as GHC
import GHC.Generics

{-# INLINE gPut_ #-}
gPut_ :: (Generic a, GPut (Rep a)) => GHC.BinHandle -> a -> IO ()
gPut_ bh a = g_put_ bh (from a)

{-# INLINE gGet #-}
gGet :: (Generic a, GGet (Rep a)) => GHC.BinHandle -> IO a
gGet bh = to <$> g_get bh

class GPut rep where
  g_put_ :: GHC.BinHandle -> rep x -> IO ()

class GGet rep where
  g_get :: GHC.BinHandle -> IO (rep x)

instance GPut U1 where
  {-# INLINE g_put_ #-}
  g_put_ _ _ = pure ()

instance GGet U1 where
  {-# INLINE g_get #-}
  g_get _ = pure U1

instance (GPut f, GPut g) => GPut (f :+: g) where
  {-# INLINE g_put_ #-}
  g_put_ bh (L1 a) = GHC.put_ bh False *> g_put_ bh a
  g_put_ bh (R1 a) = GHC.put_ bh True *> g_put_ bh a

instance (GGet f, GGet g) => GGet (f :+: g) where
  {-# INLINE g_get #-}
  g_get bh = GHC.get bh >>= \case
    False -> L1 <$> g_get bh
    True -> R1 <$> g_get bh

instance (GPut f, GPut g) => GPut (f :*: g) where
  {-# INLINE g_put_ #-}
  g_put_ bh (a :*: b) = g_put_ bh a *> g_put_ bh b

instance (GGet f, GGet g) => GGet (f :*: g) where
  {-# INLINE g_get #-}
  g_get bh = (:*:) <$> g_get bh <*> g_get bh

instance GHC.Binary c => GPut (K1 i c) where
  {-# INLINE g_put_ #-}
  g_put_ bh (K1 a) = GHC.put_ bh a

instance GHC.Binary c => GGet (K1 i c) where
  {-# INLINE g_get #-}
  g_get bh = K1 <$> GHC.get bh

instance GPut f => GPut (M1 i c f) where
  {-# INLINE g_put_ #-}
  g_put_ bh (M1 a) = g_put_ bh a

instance GGet f => GGet (M1 i c f) where
  {-# INLINE g_get #-}
  g_get bh = M1 <$> g_get bh
