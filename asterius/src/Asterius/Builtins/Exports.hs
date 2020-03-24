{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Exports
  ( exportsImports,
    exportsCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

exportsImports :: [FunctionImport]
exportsImports =
  [ FunctionImport
      { internalName = "__asterius_newHaskellCallback",
        externalModuleName = "Exports",
        externalBaseName = "newHaskellCallback",
        functionType =
          FunctionType
            { paramTypes = [F64, F64, F64, F64],
              returnTypes = [F64]
            }
      }
  ]

exportsCBits :: AsteriusModule
exportsCBits = runEDSL "newHaskellCallback" $ do
  setReturnTypes [I64]
  args <- params [I64, I64, I64, I64]
  truncUFloat64ToInt64
    <$> callImport'
      "__asterius_newHaskellCallback"
      (map convertUInt64ToFloat64 args)
      F64
    >>= emit
