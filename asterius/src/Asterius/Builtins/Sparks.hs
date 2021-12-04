{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Sparks
  ( sparksCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

sparksCBits :: AsteriusModule
sparksCBits = newSpark

-- | Create a new spark, as a result of calling @par@. This function is
-- effectively a no-op (as opposed to it's GHC counterpart, see
-- https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/Sparks.c#L54-80), since
-- non-threaded rts sparks are evaluated sequentially. Nevertheless, it is
-- required for @par@ to operate (see issue
-- https://github.com/tweag/asterius/issues/653).
newSpark :: AsteriusModule
newSpark = runEDSL "newSpark" $ do
  setReturnTypes [I32]
  _ <- params [I32, I32]
  emit $ constI32 1
