{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Scheduler
  ( schedulerImports,
  )
where

import Asterius.Types

schedulerImports :: [FunctionImport]
schedulerImports =
  [ FunctionImport
      { internalName = "tsoReportException",
        externalModuleName = "Scheduler",
        externalBaseName = "tsoReportException",
        functionType =
          FunctionType
            { paramTypes = [I32, I32],
              returnTypes = []
            }
      }
  ]
