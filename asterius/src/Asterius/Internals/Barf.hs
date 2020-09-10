{-# LANGUAGE OverloadedStrings #-}

module Asterius.Internals.Barf
  ( barf,
  )
where

import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import Data.Char

barf :: BS.ByteString -> [ValueType] -> Expression
barf msg vts =
  Block
    { name = "",
      bodys =
        [ Call
            { target = "barf_push",
              operands = [ConstI64 $ fromIntegral $ ord c],
              callReturnTypes = []
            }
          | c <- CBS.unpack msg ++ "\0"
        ]
          ++ [Unreachable],
      blockReturnTypes = vts
    }
