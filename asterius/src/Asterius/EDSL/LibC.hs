{-# LANGUAGE OverloadedStrings #-}

module Asterius.EDSL.LibC where

import Asterius.Types

memcpy :: Expression -> Expression -> Expression -> Expression
memcpy dst src n =
  Call
    { target = "memcpy",
      operands = [dst, src, n],
      callReturnTypes = [],
      callHint = Just ([AddrHint, AddrHint, NoHint], [AddrHint])
    }

memmove :: Expression -> Expression -> Expression -> Expression
memmove dst src n =
  Call
    { target = "memmove",
      operands = [dst, src, n],
      callReturnTypes = [],
      callHint = Just ([AddrHint, AddrHint, NoHint], [AddrHint])
    }
