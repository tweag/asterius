module Features
       ( features
       ) where

import Test.Framework

import qualified Features.Feature80

features :: [Test]
features = [
             Features.Feature80.main
           ]
