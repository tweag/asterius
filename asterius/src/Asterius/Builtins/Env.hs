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
            { paramTypes = [I32, I32],
              returnTypes = []
            }
      }
  ]

envCBits :: AsteriusModule
envCBits = envArgvBuf <> envGetProgArgv

-- | Data segment used to store runtime arguments. In normal native programs,
-- the arguments passed to a program @argv@ exist in special memory pages and are
-- managed by the OS kernel. Since we cannot do this here, we instead reserve a
-- data segment of 1KB of memory to be used for this purpose (see @getProgArgv@ in
-- @rts/node/default.mjs@).
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
  [argc, argv] <- params [I32, I32]
  callImport "__asterius_getProgArgv" [argc, symbol "__asterius_argv_buf"]
  storeI32 (symbol "__asterius_argv_buf") 0 (symbol "prog_name")
  storeI32 argv 0 $ symbol "__asterius_argv_buf"
