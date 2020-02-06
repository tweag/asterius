{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Process
  ( processImports,
    processCBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import qualified Data.Map.Strict as M

processImports :: [FunctionImport]
processImports =
  [ FunctionImport
      { internalName = "__asterius_runInteractiveProcess",
        externalModuleName = "process",
        externalBaseName = "runInteractiveProcess",
        functionType =
          FunctionType
            { paramTypes = replicate 15 F64,
              returnTypes = [F64]
            }
      }
  ]

processCBits :: AsteriusModule
processCBits = processErrBuf <> runInteractiveProcess

processErrBuf :: AsteriusModule
processErrBuf =
  mempty
    { staticsMap =
        M.singleton
          "__asterius_process_err_buf"
          AsteriusStatics
            { staticsType = Bytes,
              asteriusStatics = [Uninitialized 64]
            }
    }

runInteractiveProcess :: AsteriusModule
runInteractiveProcess = runEDSL "runInteractiveProcess" $ do
  setReturnTypes [I64]
  args <- params $ replicate 14 I64
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_runInteractiveProcess"
      ( map
          convertSInt64ToFloat64
          (args <> [symbol "__asterius_process_err_buf"])
      )
      F64
    >>= emit
