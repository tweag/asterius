{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Posix
  ( posixImports,
    posixCBits,
    offset_stat_mtime,
    offset_stat_size,
    offset_stat_mode,
    offset_stat_dev,
    offset_stat_ino,
  )
where

import Asterius.EDSL
import Asterius.Types
import Data.Foldable
import qualified Data.Map.Strict as M
import Foreign
import System.IO.Unsafe
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
      },
    FunctionImport
      { internalName = "__asterius_posix_fstat",
        externalModuleName = "posix",
        externalBaseName = "fstat",
        functionType =
          FunctionType
            { paramTypes = [F64, F64],
              returnTypes = [F64]
            }
      }
  ]

posixCBits :: AsteriusModule
posixCBits =
  posixOpen
    <> posixFstat
    <> posixFstatGetters
    <> posixModeGetters
    <> posixConstants
    <> posixLockFile
    <> posixUnlockFile

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

posixFstat :: AsteriusModule
posixFstat = runEDSL "__hscore_fstat" $ do
  setReturnTypes [I64]
  args <- params [I64, I64]
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_posix_fstat"
      (map convertSInt64ToFloat64 args)
      F64
    >>= emit

posixFstatGetters :: AsteriusModule
posixFstatGetters =
  mempty
    { functionMap =
        M.fromList
          [ ( k,
              Function
                { functionType =
                    FunctionType
                      { paramTypes = [I64],
                        returnTypes = [I64]
                      },
                  varTypes = [],
                  body = loadI64 GetLocal {index = 0, valueType = I64} v
                }
            )
            | (k, v) <-
                [ ("__hscore_st_mtime", offset_stat_mtime),
                  ("__hscore_st_size", offset_stat_size),
                  ("__hscore_st_mode", offset_stat_mode),
                  ("__hscore_st_dev", offset_stat_dev),
                  ("__hscore_st_ino", offset_stat_ino)
                ]
          ]
    }

posixModeGetters :: AsteriusModule
posixModeGetters =
  mempty
    { functionMap =
        M.fromList
          [ ( k,
              Function
                { functionType =
                    FunctionType
                      { paramTypes = [I64],
                        returnTypes = [I64]
                      },
                  varTypes = [],
                  body =
                    extendUInt32 $
                      ( GetLocal {index = 0, valueType = I64}
                          `andInt64` constI64 0o0170000
                      )
                        `eqInt64` constI64 v
                }
            )
            | (k, v) <-
                [ ("ghczuwrapperZC3ZCbaseZCSystemziPosixziInternalsZCSzuISSOCK", 0o140000),
                  ("ghczuwrapperZC4ZCbaseZCSystemziPosixziInternalsZCSzuISFIFO", 0o010000),
                  ("ghczuwrapperZC5ZCbaseZCSystemziPosixziInternalsZCSzuISDIR", 0o040000),
                  ("ghczuwrapperZC7ZCbaseZCSystemziPosixziInternalsZCSzuISCHR", 0o020000),
                  ("ghczuwrapperZC8ZCbaseZCSystemziPosixziInternalsZCSzuISREG", 0o100000)
                ]
          ]
    }

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
                  body = constI64 v
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

offset_stat_mtime,
  offset_stat_size,
  offset_stat_mode,
  offset_stat_dev,
  offset_stat_ino ::
    Int
(offset_stat_mtime, offset_stat_size, offset_stat_mode, offset_stat_dev, offset_stat_ino) =
  unsafePerformIO $ allocaBytes sizeof_stat $ \p -> do
    forM_ [0 .. sizeof_stat - 1] $
      \i -> pokeByteOff p i (fromIntegral i :: Word8)
    _mtime <- (.&. 0xFF) . fromEnum <$> st_mtime p
    _size <- (.&. 0xFF) . fromEnum <$> st_size p
    _mode <- (.&. 0xFF) . fromEnum <$> st_mode p
    _dev <- (.&. 0xFF) . fromEnum <$> st_dev p
    _ino <- (.&. 0xFF) . fromEnum <$> st_ino p
    pure (_mtime, _size, _mode, _dev, _ino)

posixLockFile, posixUnlockFile :: AsteriusModule
posixLockFile = runEDSL "lockFile" $ do
  setReturnTypes [I64]
  _ <- params [I64, I64, I64, I64]
  emit $ constI64 0
posixUnlockFile = runEDSL "unlockFile" $ do
  setReturnTypes [I64]
  _ <- params [I64]
  emit $ constI64 0
