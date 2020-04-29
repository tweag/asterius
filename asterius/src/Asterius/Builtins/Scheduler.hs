{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Scheduler
  ( schedulerImports,
    schedulerCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

schedulerImports :: [FunctionImport]
schedulerImports =
  [ FunctionImport
      { internalName = "__asterius_tsoReportException",
        externalModuleName = "Scheduler",
        externalBaseName = "tsoReportException",
        functionType =
          FunctionType
            { paramTypes = [F64, F64],
              returnTypes = []
            }
      }
  ]

schedulerCBits :: AsteriusModule
schedulerCBits = tsoReportException

tsoReportException :: AsteriusModule
tsoReportException = runEDSL "tsoReportException" $ do
  args <- params [I64, I64]
  callImport "__asterius_tsoReportException" (map convertUInt64ToFloat64 args)
