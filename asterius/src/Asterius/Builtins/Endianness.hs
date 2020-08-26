{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Asterius.Builtins.Endianness
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Wasm implementations of conversions between host and network byte order
-- (@htonl@, @htons@, @ntohl@, and @ntohs@). Network byte order is always Most
-- Significant Byte first (big endian). On i386 the host byte order is Least
-- Significant Byte first (little endian), which we assume always as the host
-- byte order.
module Asterius.Builtins.Endianness
  ( endiannessCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

endiannessCBits :: AsteriusModule
-- endiannessCBits = mempty -- htonl <> htons <> ntohl <> ntohs
endiannessCBits = htonl <> htons <> ntohl <> ntohs

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
byteSwap16 n = msb `orInt64` lsb
  where
    msb = (n `andInt64` constI64 0xFF) `shlInt64` constI64 8
    lsb = (n `shrUInt64` constI64 8) `andInt64` constI64 0xFF

byteSwap32 :: Expression -> Expression
byteSwap32 n = byte1 `orInt64` byte2 `orInt64` byte3 `orInt64` byte4
  where
    byte1 = (n `andInt64` constI64 0xFF) `shlInt64` constI64 24
    byte2 = (n `andInt64` constI64 0xFF00) `shlInt64` constI64 8
    byte3 = (n `andInt64` constI64 0xFF0000) `shrUInt64` constI64 8
    byte4 = (n `andInt64` constI64 0xFF000000) `shrUInt64` constI64 24
