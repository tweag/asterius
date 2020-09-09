{-# LANGUAGE OverloadedStrings #-}

module Asterius.Internals.Barf
  ( barf,
  )
where

import Asterius.Types
import Data.Char

barf :: EntitySymbol -> [ValueType] -> Expression
barf sym vts =
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
