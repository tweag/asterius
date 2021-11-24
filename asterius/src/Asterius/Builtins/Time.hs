{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Time
  ( timeImports,
  )
where

import Asterius.Types

timeImports :: [FunctionImport]
timeImports =
  [ FunctionImport
      { internalName = "clock_getres",
        externalModuleName = "time",
        externalBaseName = "clock_getres",
        functionType = FunctionType
          { paramTypes = [I32, I32],
            returnTypes = [I32]
          }
      },
    FunctionImport
      { internalName = "ghczuwrapperZC0ZCbaseZCSystemziCPUTimeziPosixziClockGetTimeZCclockzugetres",
        externalModuleName = "time",
        externalBaseName = "clock_getres",
        functionType = FunctionType
          { paramTypes = [I32, I32],
            returnTypes = [I32]
          }
      },
    FunctionImport
      { internalName = "clock_gettime",
        externalModuleName = "time",
        externalBaseName = "clock_gettime",
        functionType = FunctionType
          { paramTypes = [I32, I32],
            returnTypes = [I32]
          }
      },
    FunctionImport
      { internalName = "ghczuwrapperZC0ZCbaseZCSystemziCPUTimeziPosixziClockGetTimeZCclockzugettime",
        externalModuleName = "time",
        externalBaseName = "clock_gettime",
        functionType = FunctionType
          { paramTypes = [I32, I32],
            returnTypes = [I32]
          }
      },
    FunctionImport
      { internalName = "getMonotonicNSec",
        externalModuleName = "time",
        externalBaseName = "getMonotonicNSec",
        functionType = FunctionType {paramTypes = [], returnTypes = [I64]}
      }
  ]
