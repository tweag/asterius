{-# LANGUAGE OverloadedStrings #-}

module Asterius.Internals.Barf
  ( barf,
  )
where

import Asterius.Internals.SafeFromIntegral
import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import Data.Char

-- | Convert a @Barf@ expression into a block of calls: a call to @barf_push@
-- for each character in the error message, followed by a call to @barf_throw@
-- to issue the message, followed by @Unreachable@. NOTE: to avoid bloating,
-- use this function only during linking, when we can know whether
-- @verbose_err@ is enabled or not.
barf :: BS.ByteString -> [ValueType] -> Expression
barf msg vts =
  Block
    { name = "",
      bodys =
        [ Call
            { target = "barf_push",
              operands = [ConstI64 $ safeFromIntegral $ ord c],
              callReturnTypes = []
            }
          | c <- CBS.unpack msg
        ]
          ++ [ Call
                 { target = "barf_throw",
                   operands = [],
                   callReturnTypes = []
                 }
             ]
          ++ [Unreachable],
      blockReturnTypes = vts
    }
