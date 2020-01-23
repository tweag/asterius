{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.MD5
  ( md5CBits,
  )
where

import Asterius.EDSL
import Asterius.Types

md5CBits :: AsteriusModule
md5CBits = md5Init <> md5Update <> md5Final

md5Init, md5Update, md5Final :: AsteriusModule
md5Init = runEDSL "__hsbase_MD5Init" $ do
  ctx <- param I64
  storeI64 ctx 0 $ constI64 9223372036854775643
  storeI64 ctx 8 $ constI64 9223372036854775783
md5Update = runEDSL "__hsbase_MD5Update" $ do
  [ctx, buf, len] <- params [I64, I64, I64]
  call' "hashable_fnv_hash" [buf, len, loadI64 ctx 0] I64 >>= storeI64 ctx 0
  call' "hashable_fnv_hash" [buf, len, loadI64 ctx 8] I64 >>= storeI64 ctx 8
md5Final = runEDSL "__hsbase_MD5Final" $ do
  [digest, ctx] <- params [I64, I64]
  storeI64 digest 0 $ loadI64 ctx 0
  storeI64 digest 8 $ loadI64 ctx 8
