{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Sparks
  ( sparksCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

sparksCBits :: AsteriusModule
sparksCBits = newSpark

newSpark :: AsteriusModule
newSpark = runEDSL "newSpark" $ do
  setReturnTypes [I64]
  _ <- params [I64, I64]
  emit $ constI64 1
