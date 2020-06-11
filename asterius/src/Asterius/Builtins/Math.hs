{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Math
  ( mathCBits,
  )
where

-- All the relevant @__asterius_<fn>_F64@ imports can be found in
-- @Asterius.Builtins.rtsFunctionImports@.

import Asterius.EDSL
import Asterius.Types

mathCBits :: AsteriusModule
mathCBits =
  mconcat
    [ runEDSL (mkEntitySymbol op) $ do
        setReturnTypes [F64]
        x <- param F64
        callImport' ("__asterius_" <> op <> "_F64") [x] F64 >>= emit
      | op <-
          [ "sin",
            "cos",
            "tan",
            "sinh",
            "cosh",
            "tanh",
            "asin",
            "acos",
            "atan",
            "log",
            "exp"
          ]
    ]
