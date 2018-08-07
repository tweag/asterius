module Main where

import Data.List (nub, nubBy)
import Data.Containers.ListUtils
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Function (Fun, apply)
import Test.QuickCheck.Poly (A, OrdA, B, OrdB, C)

main :: IO ()
main = defaultMain
         [ testProperty "nubOrd" prop_nubOrd
         , testProperty "nubOrdOn" prop_nubOrdOn
         , testProperty "nubOrdOn fusion" prop_nubOrdOnFusion
         , testProperty "nubInt" prop_nubInt
         , testProperty "nubIntOn" prop_nubIntOn
         , testProperty "nubIntOn fusion" prop_nubIntOnFusion
         ]


prop_nubOrd :: [OrdA] -> Property
prop_nubOrd xs = nubOrd xs === nub xs

prop_nubInt :: [Int] -> Property
prop_nubInt xs = nubInt xs === nub xs

prop_nubOrdOn :: Fun A OrdB -> [A] -> Property
prop_nubOrdOn f' xs =
  nubOrdOn f xs === nubBy (\x y -> f x == f y) xs
  where f = apply f'

prop_nubIntOn :: Fun A Int -> [A] -> Property
prop_nubIntOn f' xs =
  nubIntOn f xs === nubBy (\x y -> f x == f y) xs
  where f = apply f'

prop_nubOrdOnFusion :: Fun B C
                    -> Fun B OrdB
                    -> Fun A B
                    -> [A] -> Property
prop_nubOrdOnFusion f' g' h' xs =
  (map f . nubOrdOn g . map h $ xs)
    === (map f . nubBy (\x y -> g x == g y) . map h $ xs)
  where
    f = apply f'
    g = apply g'
    h = apply h'

prop_nubIntOnFusion :: Fun B C
                    -> Fun B Int
                    -> Fun A B
                    -> [A] -> Property
prop_nubIntOnFusion f' g' h' xs =
  (map f . nubIntOn g . map h $ xs)
    === (map f . nubBy (\x y -> g x == g y) . map h $ xs)
  where
    f = apply f'
    g = apply g'
    h = apply h'
