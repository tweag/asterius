{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Asterius.Builtins.Endianness
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Wasm implementations of conversions between host (little-endian) and network
-- (big-endian) byte order: @htonl@, @htons@, @ntohl@, and @ntohs@.
module Asterius.Builtins.Endianness
  ( endiannessCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

endiannessCBits :: AsteriusModule
endiannessCBits = mempty
-- endiannessCBits = htonl <> htons <> ntohl <> ntohs

-- | @uint32_t htonl(uint32_t hostlong);@
htonl :: AsteriusModule
htonl = runEDSL "htonl" $ do
  setReturnTypes [I64]
  hostlong <- param I64
  emit $ byteSwap32 hostlong

-- | @uint16_t htons(uint16_t hostshort);@
htons :: AsteriusModule
htons = runEDSL "htons" $ do
  setReturnTypes [I64]
  hostshort <- param I64
  emit $ byteSwap16 hostshort

-- | @uint32_t ntohl(uint32_t netlong);@
ntohl :: AsteriusModule
ntohl = runEDSL "ntohl" $ do
  setReturnTypes [I64]
  netlong <- param I64
  emit $ byteSwap32 netlong

-- | @uint16_t ntohs(uint16_t netshort);@
ntohs :: AsteriusModule
ntohs = runEDSL "ntohs" $ do
  setReturnTypes [I64]
  netshort <- param I64
  emit $ byteSwap16 netshort

byteSwap16 :: Expression -> Expression
byteSwap16 n =
  ((n `andInt64` constI64 0xFF) `shlInt64` constI64 8)
    `orInt64` ((n `shrUInt64` constI64 8) `andInt64` constI64 0xFF)

byteSwap32 :: Expression -> Expression
byteSwap32 n =
  ((n `andInt64` constI64 0xFF) `shlInt64` constI64 24)
    `orInt64` ((n `andInt64` constI64 0xFF00) `shlInt64` constI64 8)
    `orInt64` ((n `andInt64` constI64 0xFF0000) `shrUInt64` constI64 8)
    `orInt64` ((n `andInt64` constI64 0xFF000000) `shrUInt64` constI64 24)
