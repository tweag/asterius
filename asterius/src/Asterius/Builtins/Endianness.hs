{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Asterius.Builtins.Endianness
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Wasm implementations of byte-swapping functions (@hs_bswap16@, @hs_bswap32@,
-- and @hs_bswap64@), and conversions between host (little-endian) and network
-- (big-endian) byte order (@htonl@, @htons@, @ntohl@, and @ntohs@).
module Asterius.Builtins.Endianness
  ( endiannessCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

endiannessCBits :: AsteriusModule
endiannessCBits =
  hs_bswap16 <> hs_bswap32 <> hs_bswap64 <> htonl <> htons <> ntohl <> ntohs

-- ----------------------------------------------------------------------------

-- | @uint32_t htonl(uint32_t hostlong);@
htonl :: AsteriusModule
htonl = runEDSL "htonl" $ do
  setReturnTypes [I64]
  hostlong <- param I64
  call' "hs_bswap32" [hostlong] I64 >>= emit

-- | @uint16_t htons(uint16_t hostshort);@
htons :: AsteriusModule
htons = runEDSL "htons" $ do
  setReturnTypes [I64]
  hostshort <- param I64
  call' "hs_bswap16" [hostshort] I64 >>= emit

-- | @uint32_t ntohl(uint32_t netlong);@
ntohl :: AsteriusModule
ntohl = runEDSL "ntohl" $ do
  setReturnTypes [I64]
  netlong <- param I64
  call' "hs_bswap32" [netlong] I64 >>= emit

-- | @uint16_t ntohs(uint16_t netshort);@
ntohs :: AsteriusModule
ntohs = runEDSL "ntohs" $ do
  setReturnTypes [I64]
  netshort <- param I64
  call' "hs_bswap16" [netshort] I64 >>= emit

-- ----------------------------------------------------------------------------

-- | @extern StgWord16 hs_bswap16(StgWord16 x);@
hs_bswap16 :: AsteriusModule
hs_bswap16 = runEDSL "hs_bswap16" $ do
  setReturnTypes [I64]
  x <- param I64
  emit $ byteSwap16 x

-- | @extern StgWord32 hs_bswap32(StgWord32 x);@
hs_bswap32 :: AsteriusModule
hs_bswap32 = runEDSL "hs_bswap32" $ do
  setReturnTypes [I64]
  x <- param I64
  emit $ byteSwap32 x

-- | @extern StgWord64 hs_bswap64(StgWord64 x);@
hs_bswap64 :: AsteriusModule
hs_bswap64 = runEDSL "hs_bswap64" $ do
  setReturnTypes [I64]
  x <- param I64
  emit $ byteSwap64 x

-- ----------------------------------------------------------------------------

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

byteSwap64 :: Expression -> Expression
byteSwap64 n =
  byte1
    `orInt64` byte2
    `orInt64` byte3
    `orInt64` byte4
    `orInt64` byte5
    `orInt64` byte6
    `orInt64` byte7
    `orInt64` byte8
  where
    byte1 = (n `andInt64` constI64 0xFF) `shlInt64` constI64 56
    byte2 = (n `andInt64` constI64 0xFF00) `shlInt64` constI64 40
    byte3 = (n `andInt64` constI64 0xFF0000) `shlInt64` constI64 24
    byte4 = (n `andInt64` constI64 0xFF000000) `shlInt64` constI64 8
    byte5 = (n `andInt64` constI64 0xFF00000000) `shrUInt64` constI64 8
    byte6 = (n `andInt64` constI64 0xFF0000000000) `shrUInt64` constI64 24
    byte7 = (n `andInt64` constI64 0xFF000000000000) `shrUInt64` constI64 40
    byte8 = (n `andInt64` constI64 0xFF00000000000000) `shrUInt64` constI64 56
