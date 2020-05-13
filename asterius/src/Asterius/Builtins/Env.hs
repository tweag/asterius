{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Env
  ( envImports,
    envCBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM

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
envCBits = envArgvBuf <> envGetProgArgv

envArgvBuf :: AsteriusModule
envArgvBuf =
  mempty
    { staticsMap =
        SM.singleton
          "__asterius_argv_buf"
          AsteriusStatics
            { staticsType = Bytes,
              asteriusStatics = [Uninitialized 1024]
            }
    }

envGetProgArgv :: AsteriusModule
envGetProgArgv = runEDSL "getProgArgv" $ do
  [argc, argv] <- params [I64, I64]
  callImport "__asterius_getProgArgv" $
    map convertUInt64ToFloat64 [argc, symbol "__asterius_argv_buf"]
  storeI64 (symbol "__asterius_argv_buf") 0 (symbol "prog_name")
  storeI64 argv 0 $ symbol "__asterius_argv_buf"
