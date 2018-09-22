{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Asterius.Internals.FList
  ( FList
  , cons
  , snoc
  , concat
  ) where

import Data.Binary
import GHC.Exts
import Prelude hiding (concat)

data FList a where
  Empty :: FList a
  Singleton :: a -> FList a
  Cons :: a -> FList a -> FList a
  Snoc :: FList a -> a -> FList a
  Append :: FList a -> FList a -> FList a
  FromFoldable :: Foldable t => t a -> FList a
  Map :: (a -> b) -> FList a -> FList b
  Join :: FList (FList a) -> FList a

instance Semigroup (FList a) where
  {-# INLINE (<>) #-}
  Empty <> l1 = l1
  l0 <> Empty = l0
  Singleton a <> l1 = Cons a l1
  l0 <> Singleton a = Snoc l0 a
  l0 <> l1 = Append l0 l1

instance Monoid (FList a) where
  {-# INLINE mempty #-}
  mempty = Empty

instance Foldable FList where
  foldr f b l =
    case l of
      Empty -> b
      Singleton a -> f a b
      Cons a l0 -> f a (foldr f b l0)
      Snoc l0 a -> foldr f (f a b) l0
      Append l0 l1 -> foldr f (foldr f b l1) l0
      FromFoldable l0 -> Prelude.foldr f b l0
      Map g l0 -> foldr (f . g) b l0
      Join l0 -> foldr (flip (foldr f)) b l0
  null l =
    case l of
      Empty -> True
      Singleton {} -> False
      Cons {} -> False
      Snoc {} -> False
      Append l0 l1 -> null l0 && null l1
      FromFoldable l0 -> null l0
      Map _ l0 -> null l0
      Join l0 -> and (fmap null l0)

instance Functor FList where
  {-# INLINE fmap #-}
  fmap f l =
    case l of
      Empty -> Empty
      Singleton a -> Singleton (f a)
      Map g l0 -> Map (f . g) l0
      _ -> Map f l

instance Applicative FList where
  {-# INLINE pure #-}
  pure = Singleton
  {-# INLINE (<*>) #-}
  f <*> a = Join (fmap (`fmap` a) f)

instance Monad FList where
  {-# INLINE (>>=) #-}
  l >>= f = Join (fmap f l)

instance IsList (FList a) where
  type Item (FList a) = a
  {-# INLINE fromList #-}
  fromList = FromFoldable
  {-# INLINE toList #-}
  toList = foldr (:) []

instance Show a => Show (FList a) where
  {-# INLINE showsPrec #-}
  showsPrec i = showsPrec i . toList

instance Binary a => Binary (FList a) where
  get = fromList <$> get
  put = put . toList

{-# INLINE cons #-}
cons :: a -> FList a -> FList a
cons = Cons

{-# INLINE snoc #-}
snoc :: FList a -> a -> FList a
snoc = Snoc

{-# INLINE concat #-}
concat :: FList (FList a) -> FList a
concat = Join
