{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Barf
  ( barfImports,
  )
where

import Asterius.Types

barfImports :: [FunctionImport]
barfImports =
  [ FunctionImport
      { internalName = "barf",
        externalModuleName = "ExceptionHelper",
        externalBaseName = "barf",
        functionType = FunctionType {paramTypes = [I32], returnTypes = []}
      },
    FunctionImport
      { internalName = "barf_push",
        externalModuleName = "ExceptionHelper",
        externalBaseName = "barf_push",
        functionType = FunctionType {paramTypes = [I32], returnTypes = []}
      },
    FunctionImport
      { internalName = "barf_signal",
        externalModuleName = "ExceptionHelper",
        externalBaseName = "barf_signal",
        functionType = FunctionType {paramTypes = [I32], returnTypes = []}
      }
  ]
