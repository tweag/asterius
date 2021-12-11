{-# LANGUAGE OverloadedStrings #-}

module Asterius.EDSL.LibC where

import Asterius.Types

memcmp :: Expression -> Expression -> Expression -> Expression
memcmp lhs rhs n =
  Call
    { target = "memcmp",
      operands = [lhs, rhs, n],
      callReturnTypes = [I32]
    }

memcpy :: Expression -> Expression -> Expression -> Expression
memcpy dst src n =
  Drop $ Call
    { target = "memcpy",
      operands = [dst, src, n],
      callReturnTypes = [I32]
    }

memmove :: Expression -> Expression -> Expression -> Expression
memmove dst src n =
  Drop $ Call
    { target = "memmove",
      operands = [dst, src, n],
      callReturnTypes = [I32]
    }

memset :: Expression -> Expression -> Expression -> Expression
memset dst c n =
  Drop $ Call
    { target = "memset",
      operands = [dst, c, n],
      callReturnTypes = [I32]
    }
