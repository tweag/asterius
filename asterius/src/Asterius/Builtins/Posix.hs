{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Posix
  ( posixImports,
    posixCBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import qualified Data.Map.Strict as M
import System.Posix.Internals

posixImports :: [FunctionImport]
posixImports =
  [ FunctionImport
      { internalName = "__asterius_posix_open",
        externalModuleName = "posix",
        externalBaseName = "open",
        functionType =
          FunctionType
            { paramTypes = [F64, F64, F64],
              returnTypes = [F64]
            }
      }
  ]

posixCBits :: AsteriusModule
posixCBits = posixOpen <> posixConstants

posixOpen :: AsteriusModule
posixOpen = runEDSL "__hscore_open" $ do
  setReturnTypes [I64]
  args <- params [I64, I64, I64]
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_posix_open"
      (map convertSInt64ToFloat64 args)
      F64
    >>= emit

posixConstants :: AsteriusModule
posixConstants =
  mempty
    { functionMap =
        M.fromList
          [ ( k,
              Function
                { functionType =
                    FunctionType
                      { paramTypes = [],
                        returnTypes = [I64]
                      },
                  varTypes = [],
                  body = ConstI64 v
                }
            )
            | (k, v) <-
                [ ("__hscore_sizeof_stat", fromIntegral sizeof_stat),
                  ("__hscore_o_rdonly", fromIntegral o_RDONLY),
                  ("__hscore_o_wronly", fromIntegral o_WRONLY),
                  ("__hscore_o_rdwr", fromIntegral o_RDWR),
                  ("__hscore_o_append", fromIntegral o_APPEND),
                  ("__hscore_o_creat", fromIntegral o_CREAT),
                  ("__hscore_o_excl", fromIntegral o_EXCL),
                  ("__hscore_o_trunc", fromIntegral o_TRUNC),
                  ("__hscore_o_noctty", fromIntegral o_NOCTTY),
                  ("__hscore_o_nonblock", fromIntegral o_NONBLOCK),
                  ("__hscore_o_binary", fromIntegral o_BINARY)
                ]
          ]
    }
