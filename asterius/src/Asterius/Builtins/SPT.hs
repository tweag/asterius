{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.SPT
  ( sptImports,
    sptCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

sptImports :: [FunctionImport]
sptImports =
  [ FunctionImport
      { internalName = "__asterius_hs_spt_lookup",
        externalModuleName = "StaticPtr",
        externalBaseName = "hs_spt_lookup",
        functionType =
          FunctionType
            { paramTypes = [I32, I32, I32, I32],
              returnTypes = [F64]
            }
      },
    FunctionImport
      { internalName = "__asterius_hs_spt_key_count",
        externalModuleName = "StaticPtr",
        externalBaseName = "hs_spt_key_count",
        functionType = FunctionType {paramTypes = [], returnTypes = [F64]}
      },
    FunctionImport
      { internalName = "__asterius_hs_spt_keys",
        externalModuleName = "StaticPtr",
        externalBaseName = "hs_spt_keys",
        functionType =
          FunctionType
            { paramTypes = [F64, F64],
              returnTypes = [F64]
            }
      }
  ]

sptCBits :: AsteriusModule
sptCBits = sptLookup <> sptKeyCount <> sptKeys

sptLookup :: AsteriusModule
sptLookup = runEDSL "hs_spt_lookup" $ do
  setReturnTypes [I64]
  [w0, w1] <- params [I64, I64]
  truncUFloat64ToInt64
    <$> callImport'
      "__asterius_hs_spt_lookup"
      [ wrapInt64 w0,
        wrapInt64 $ w0 `shrUInt64` constI64 32,
        wrapInt64 w1,
        wrapInt64 $ w1 `shrUInt64` constI64 32
      ]
      F64
    >>= emit

sptKeyCount :: AsteriusModule
sptKeyCount = runEDSL "hs_spt_key_count" $ do
  setReturnTypes [I64]
  truncUFloat64ToInt64
    <$> callImport' "__asterius_hs_spt_key_count" [] F64
    >>= emit

sptKeys :: AsteriusModule
sptKeys = runEDSL "hs_spt_keys" $ do
  setReturnTypes [I64]
  args <- params [I64, I64]
  truncUFloat64ToInt64
    <$> callImport'
      "__asterius_hs_spt_keys"
      (map convertUInt64ToFloat64 args)
      F64
    >>= emit
