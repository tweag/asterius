{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Env
  ( envImports,
    envCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

envImports :: [FunctionImport]
envImports =
  [ FunctionImport
      { internalName = "__asterius_getProgArgv",
        externalModuleName = "posix",
        externalBaseName = "getProgArgv",
        functionType =
          FunctionType
            { paramTypes = [F64, F64],
              returnTypes = []
            }
      }
  ]

envCBits :: AsteriusModule
envCBits = envGetProgArgv

envGetProgArgv :: AsteriusModule
envGetProgArgv = runEDSL "getProgArgv" $ do
  args <- params [I64, I64]
  callImport "__asterius_getProgArgv" $ map convertUInt64ToFloat64 args
