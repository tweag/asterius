{-# LANGUAGE OverloadedStrings #-}

module Asterius.Internals.Barf
  ( barf,
  )
where

import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import Data.Char

-- | Convert a @Barf@ expression into a block of calls: a call to @barf_push@
-- for each character in the error message, followed by a call to @barf_signal@
-- to issue the message, followed by @Unreachable@. NOTE: to avoid bloating,
-- use this function only during linking, when we can know whether
-- @verbose_err@ is enabled or not.
barf :: BS.ByteString -> [ValueType] -> Expression
barf msg vts =
  Block
    { name = "",
      bodys =
        [ CallImport
            { target' = "barf_push",
              operands = [ConstI32 $ fromIntegral $ ord c],
              callImportReturnTypes = []
            }
          | c <- CBS.unpack msg
        ]
          <> [ CallImport
                 { target' = "barf_signal",
                   operands = [ConstI32 1],
                   callImportReturnTypes = []
                 },
               Unreachable
             ],
      blockReturnTypes = vts
    }
