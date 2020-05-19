{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Malloc
  ( mallocCBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import Language.Haskell.GHC.Toolkit.Constants

mallocCBits :: AsteriusModule
mallocCBits = malloc <> free

malloc :: AsteriusModule
malloc = runEDSL "malloc" $ do
  setReturnTypes [I64]
  n <- param I64
  c <-
    call'
      "allocatePinned"
      [ mainCapability,
        roundup_bytes_to_words_expr $
          constI64 (sizeof_StgArrBytes + 8)
            `addInt64` n
      ]
      I64
  storeI64 c 0 $ symbol "stg_ARR_WORDS_info"
  storeI64 c offset_StgArrBytes_bytes $ constI64 8 `addInt64` n
  sp <- call' "getStablePtr" [c] I64
  storeI64 c offset_StgArrBytes_payload sp
  emit $ c `addInt64` constI64 (offset_StgArrBytes_payload + 8)

free :: AsteriusModule
free = runEDSL "free" $ do
  p <- param I64
  call "freeStablePtr" [loadI64 (p `subInt64` constI64 8) 0]

roundup_bytes_to_words_expr :: Expression -> Expression
roundup_bytes_to_words_expr n =
  (n `addInt64` constI64 7) `divUInt64` constI64 8
