{-# LANGUAGE OverloadedStrings #-}

module Asterius.Internals.Barf
  ( barf,
    barfPush,
  )
where

import Asterius.Types
import Data.Char

barf :: EntitySymbol -> [ValueType] -> Expression
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

barfPush :: EntitySymbol -> [ValueType] -> Expression
barfPush sym vts =
  Block
    { name = "",
      bodys =
        [ Call
            { target = "barf_push",
              operands = [ConstI64 $ fromIntegral $ ord c],
              callReturnTypes = []
            }
          | c <- ("Cannot find function " ++ show sym ++ "\0")
        ]
          ++ [Unreachable],
      blockReturnTypes = vts
    }
