{-# LANGUAGE OverloadedStrings #-}

module Asterius.Internals.Barf
  ( barf,
  )
where

import Asterius.Types

barf :: AsteriusEntitySymbol -> [ValueType] -> Expression
barf sym [] = Call
  { target = "barf",
    operands =
      [ Symbol
          { unresolvedSymbol = "__asterius_barf_" <> sym,
            symbolOffset = 0
          }
      ],
    callReturnTypes = []
  }
barf sym vts = Block
  { name = "",
    bodys = [barf sym [], Unreachable],
    blockReturnTypes = vts
  }
