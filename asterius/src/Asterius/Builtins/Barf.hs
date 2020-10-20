{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Barf
  ( barfImports,
    barfCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

barfImports :: [FunctionImport]
barfImports =
  [ FunctionImport
      { internalName = "__asterius_barf",
        externalModuleName = "ExceptionHelper",
        externalBaseName = "barf",
        functionType = FunctionType {paramTypes = [F64], returnTypes = []}
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

barfCBits :: AsteriusModule
barfCBits = barfFunction

barfFunction :: AsteriusModule
barfFunction = runEDSL "barf" $ do
  s <- param I64
  callImport "__asterius_barf" [convertUInt64ToFloat64 s]
