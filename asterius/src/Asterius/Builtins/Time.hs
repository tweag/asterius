{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Time
  ( timeImports,
    timeCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

timeImports :: [FunctionImport]
timeImports =
  [ FunctionImport
      { internalName = "__asterius_clock_getres",
        externalModuleName = "time",
        externalBaseName = "clock_getres",
        functionType = FunctionType
          { paramTypes = [F64, F64],
            returnTypes = [F64]
          }
      },
    FunctionImport
      { internalName = "__asterius_clock_gettime",
        externalModuleName = "time",
        externalBaseName = "clock_gettime",
        functionType = FunctionType
          { paramTypes = [F64, F64],
            returnTypes = [F64]
          }
      },
    FunctionImport
      { internalName = "__asterius_getMonotonicNSec",
        externalModuleName = "time",
        externalBaseName = "getMonotonicNSec",
        functionType = FunctionType {paramTypes = [], returnTypes = [F64]}
      }
  ]

timeCBits :: AsteriusModule
timeCBits =
  clockGetRes
    <> clockGetTime
    <> capiClockGetRes
    <> capiClockGetTime
    <> getMonotonicNSec

clockGetRes,
  clockGetTime,
  capiClockGetRes,
  capiClockGetTime,
  getMonotonicNSec ::
    AsteriusModule
clockGetRes = runEDSL "clock_getres" $ do
  setReturnTypes [I64]
  args <- params [I64, I64]
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_clock_getres"
      (map convertUInt64ToFloat64 args)
      F64
    >>= emit
clockGetTime = runEDSL "clock_gettime" $ do
  setReturnTypes [I64]
  args <- params [I64, I64]
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_clock_gettime"
      (map convertUInt64ToFloat64 args)
      F64
    >>= emit
capiClockGetRes =
  runEDSL
    "ghczuwrapperZC0ZCbaseZCSystemziCPUTimeziPosixziClockGetTimeZCclockzugetres"
    $ do
      setReturnTypes [I64]
      args <- params [I64, I64]
      call' "clock_getres" args I64 >>= emit
capiClockGetTime =
  runEDSL
    "ghczuwrapperZC0ZCbaseZCSystemziCPUTimeziPosixziClockGetTimeZCclockzugettime"
    $ do
      setReturnTypes [I64]
      args <- params [I64, I64]
      call' "clock_gettime" args I64 >>= emit
getMonotonicNSec = runEDSL "getMonotonicNSec" $ do
  setReturnTypes [I64]
  truncUFloat64ToInt64
    <$> callImport' "__asterius_getMonotonicNSec" [] F64
    >>= emit
