{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Exports
  ( exportsImports,
  )
where

import Asterius.Types

exportsImports :: [FunctionImport]
exportsImports =
  [ FunctionImport
      { internalName = "newHaskellCallback",
        externalModuleName = "Exports",
        externalBaseName = "newHaskellCallback",
        functionType =
          FunctionType
            { paramTypes = [I32, I32, I32, I32, I32],
              returnTypes = [I32]
            }
      },
    FunctionImport
      { internalName = "freeHaskellCallback",
        externalModuleName = "Exports",
        externalBaseName = "freeHaskellCallback",
        functionType = FunctionType {paramTypes = [I32], returnTypes = []}
      }
  ]
