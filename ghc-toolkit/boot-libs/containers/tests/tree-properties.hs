{-# LANGUAGE CPP #-}

import Data.Tree as T

import Control.Applicative (Const(Const, getConst), pure, (<$>), (<*>), liftA2)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Function (Fun (..), apply)
import Test.QuickCheck.Poly (A, B, C)
import Control.Monad.Fix (MonadFix (..))
import Control.Monad (ap)

default (Int)

main :: IO ()
main = defaultMain
         [
           testProperty "monad_id1"                prop_monad_id1
         , testProperty "monad_id2"                prop_monad_id2
         , testProperty "monad_assoc"              prop_monad_assoc
         , testProperty "ap_ap"                    prop_ap_ap
         , testProperty "ap_liftA2"                prop_ap_liftA2
         , testProperty "monadFix_ls"              prop_monadFix_ls
         ]

{--------------------------------------------------------------------
  Arbitrary trees
--------------------------------------------------------------------}


-- This instance isn't balanced very well; the trees will probably tend
-- to lean left. But it's better than nothing and we can fix it later.
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized (fmap snd . arbtree)
    where
      arbtree :: Arbitrary a => Int -> Gen (Int, Tree a)
      arbtree 0 = fmap ((,) 1) $ Node <$> arbitrary <*> pure []
      arbtree n = do
        root <- arbitrary
        num_children <- choose (0, n - 1)
        (st, tl) <- go num_children
        return (1+st, Node root tl)

      go 0 = pure (0, [])
      go n = do
        (sh, hd) <- arbtree n
        (st, tl) <- go (n - sh)
        pure (sh + st, hd : tl)

#if defined(__GLASGOW_HASKELL__)
  shrink = genericShrink
#endif

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

----------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------

apply2 :: Fun (a, b) c -> a -> b -> c
apply2 f a b = apply f (a, b)

prop_ap_ap :: Tree (Fun A B) -> Tree A -> Property
prop_ap_ap fs xs = (apply <$> fs <*> xs) === ((apply <$> fs) `ap` xs)

prop_ap_liftA2 :: Fun (A, B) C -> Tree A -> Tree B -> Property
prop_ap_liftA2 f as bs = (apply2 f <$> as <*> bs) === liftA2 (apply2 f) as bs

prop_monad_id1 :: Tree A -> Property
prop_monad_id1 t = (t >>= pure) === t

prop_monad_id2 :: A -> Fun A (Tree B) -> Property
prop_monad_id2 a f = (pure a >>= apply f) === apply f a

prop_monad_assoc :: Tree A -> Fun A (Tree B) -> Fun B (Tree C) -> Property
prop_monad_assoc ta atb btc =
  ((ta >>= apply atb) >>= apply btc)
  ===
  (ta >>= \a -> apply atb a >>= apply btc)

-- The left shrinking law
--
-- This test is kind of wonky and unprincipled, because it's
-- rather tricky to construct test cases!
-- This is the most important MonadFix law to test because it's the
-- least intuitive by far, and because it's the only one that's
-- sensitive to the Monad instance.
prop_monadFix_ls :: Int -> Tree Int -> Fun Int (Tree Int) -> Property
prop_monadFix_ls val ta ti =
  fmap ($val) (mfix (\x -> ta >>= \y -> f x y))
  ===
  fmap ($val) (ta >>= \y -> mfix (\x -> f x y))
  where
    fact :: Int -> (Int -> Int) -> Int -> Int
    fact x _ 0 = x + 1
    fact x f n = x + n * f ((n - 1) `mod` 23)

    f :: (Int -> Int) -> Int -> Tree (Int -> Int)
    f q y = let t = apply ti y
            in fmap (\w -> fact w q) t
