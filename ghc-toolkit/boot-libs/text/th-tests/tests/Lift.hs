{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lift
  ( tests
  )
  where

import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Language.Haskell.TH.Syntax (lift)
import Test.HUnit (assertBool, assertEqual, assertFailure)
import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

tests :: F.Test
tests = F.testGroup "TH lifting Text"
  [ F.testCase "strict" $ assertEqual "strict"
      $(lift ("foo" :: S.Text))
      ("foo" :: S.Text)
  , F.testCase "lazy" $ assertEqual "lazy"
      $(lift ("foo" :: L.Text))
      ("foo" :: L.Text)
  ]
