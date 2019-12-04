{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Hashable
  ( hashableCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

hashableCBits :: AsteriusModule
hashableCBits = hashableFNVHash <> hashableFNVHashOffset

hashableFNVHash, hashableFNVHashOffset :: AsteriusModule
hashableFNVHash = runEDSL "hashable_fnv_hash" $ do
  setReturnTypes [I64]
  [str, len, salt] <- params [I64, I64, I64]
  hash <- i64MutLocal
  putLVal hash salt
  i <- i64MutLocal
  putLVal i $ constI64 0
  whileLoop [] (getLVal i `ltUInt64` len) $ do
    putLVal hash $
      ( getLVal hash
          `xorInt64` extendUInt32 (loadI8 (str `addInt64` getLVal i) 0)
      )
        `mulInt64` constI64 16777619
    putLVal i $ getLVal i `addInt64` constI64 1
  emit $ getLVal hash
hashableFNVHashOffset = runEDSL "hashable_fnv_hash_offset" $ do
  setReturnTypes [I64]
  [str, offset', len, salt] <- params [I64, I64, I64, I64]
  call' "hashable_fnv_hash" [str `addInt64` offset', len, salt] I64 >>= emit
