{-# LANGUAGE OverloadedStrings #-}

module Asterius.EDSL.LibC where

import Asterius.Types

memcmp :: Expression -> Expression -> Expression -> Expression
memcmp lhs rhs n =
  Call
    { target = "memcmp",
      operands = [lhs, rhs, n],
      callReturnTypes = [I32],
      callHint = Just ([AddrHint, AddrHint, NoHint], [SignedHint])
    }

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

memset :: Expression -> Expression -> Expression -> Expression
memset dst c n =
  Call
    { target = "memset",
      operands = [dst, c, n],
      callReturnTypes = [],
      callHint = Just ([AddrHint, NoHint, NoHint], [AddrHint])
    }
