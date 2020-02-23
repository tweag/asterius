{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.TSO
  ( tsoImports,
    tsoCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

tsoImports :: [FunctionImport]
tsoImports =
  [ FunctionImport
      { internalName = "__asterius_tso_init",
        externalModuleName = "Scheduler",
        externalBaseName = "tso_init",
        functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      }
  ]

tsoCBits :: AsteriusModule
tsoCBits = tsoInit

tsoInit :: AsteriusModule
tsoInit = runEDSL "tso_init" $ do
  tso <- param I64
  callImport "__asterius_tso_init" [convertSInt64ToFloat64 tso]
