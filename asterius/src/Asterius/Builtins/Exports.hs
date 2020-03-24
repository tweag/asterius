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
      },
    FunctionImport
      { internalName = "__asterius_freeHaskellCallback",
        externalModuleName = "Exports",
        externalBaseName = "freeHaskellCallback",
        functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      }
  ]

exportsCBits :: AsteriusModule
exportsCBits = newHaskellCallback <> freeHaskellCallback

newHaskellCallback :: AsteriusModule
newHaskellCallback = runEDSL "newHaskellCallback" $ do
  setReturnTypes [I64]
  args <- params [I64, I64, I64, I64]
  truncUFloat64ToInt64
    <$> callImport'
      "__asterius_newHaskellCallback"
      (map convertUInt64ToFloat64 args)
      F64
    >>= emit

freeHaskellCallback :: AsteriusModule
freeHaskellCallback = runEDSL "freeHaskellCallback" $ do
  arg <- param I64
  callImport "__asterius_freeHaskellCallback" [convertUInt64ToFloat64 arg]
