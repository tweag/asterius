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
      { internalName = "__asterius_barf_push",
        externalModuleName = "ExceptionHelper",
        externalBaseName = "barf_push",
        functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      },
    FunctionImport
      { internalName = "__asterius_barf_throw",
        externalModuleName = "ExceptionHelper",
        externalBaseName = "barf_throw",
        functionType = FunctionType {paramTypes = [], returnTypes = []}
      }
  ]

barfCBits :: AsteriusModule
barfCBits = barfFunction <> barfPushFunction <> barfThrowFunction

barfFunction :: AsteriusModule
barfFunction = runEDSL "barf" $ do
  s <- param I64
  callImport "__asterius_barf" [convertUInt64ToFloat64 s]

barfPushFunction :: AsteriusModule
barfPushFunction = runEDSL "barf_push" $ do
  s <- param I64
  callImport "__asterius_barf_push" [convertUInt64ToFloat64 s]

barfThrowFunction :: AsteriusModule
barfThrowFunction = runEDSL "barf_throw" $ do
  _ <- params []
  callImport "__asterius_barf_throw" []
