-- | Provides a simple main function which runs all the tests
--
module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Lift

main :: IO ()
main = defaultMain [Lift.tests]
