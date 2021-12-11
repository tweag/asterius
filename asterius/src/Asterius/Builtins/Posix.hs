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
import qualified Asterius.Types.SymbolMap as SM
import qualified Data.ByteString as BS
import Data.Foldable
import Foreign
import System.IO.Unsafe
import System.Posix.Internals

posixImports :: [FunctionImport]
posixImports =
  [ FunctionImport
      { internalName = "__hscore_get_errno",
        externalModuleName = "posix",
        externalBaseName = "get_errno",
        functionType = FunctionType {paramTypes = [], returnTypes = [I32]}
      },
    FunctionImport
      { internalName = "__hscore_set_errno",
        externalModuleName = "posix",
        externalBaseName = "set_errno",
        functionType = FunctionType {paramTypes = [I32], returnTypes = []}
      },
    FunctionImport
      { internalName = "__hscore_open",
        externalModuleName = "posix",
        externalBaseName = "open",
        functionType =
          FunctionType
            { paramTypes = [I32, I32, I32],
              returnTypes = [I32]
            }
      },
    FunctionImport
      { internalName = "close",
        externalModuleName = "posix",
        externalBaseName = "close",
        functionType = FunctionType {paramTypes = [I32], returnTypes = [I32]}
      },
    FunctionImport
      { internalName = "__hscore_ftruncate",
        externalModuleName = "posix",
        externalBaseName = "ftruncate",
        functionType =
          FunctionType
            { paramTypes = [I32, I64],
              returnTypes = [I32]
            }
      },
    FunctionImport
      { internalName = "__hscore_stat",
        externalModuleName = "posix",
        externalBaseName = "stat",
        functionType =
          FunctionType
            { paramTypes = [I32, I32],
              returnTypes = [I32]
            }
      },
    FunctionImport
      { internalName = "__hscore_fstat",
        externalModuleName = "posix",
        externalBaseName = "fstat",
        functionType =
          FunctionType
            { paramTypes = [I32, I32],
              returnTypes = [I32]
            }
      },
    FunctionImport
      { internalName = "__asterius_posix_opendir",
        externalModuleName = "posix",
        externalBaseName = "opendir",
        functionType = FunctionType {paramTypes = [I32], returnTypes = [I32]}
      },
    FunctionImport
      { internalName = "__asterius_posix_readdir",
        externalModuleName = "posix",
        externalBaseName = "readdir",
        functionType =
          FunctionType
            { paramTypes = [I32, I32],
              returnTypes = [I32]
            }
      }
  ]

posixCBits :: AsteriusModule
posixCBits =
  posixFstatGetters
    <> posixModeGetters
    <> posixConstants
    <> posixLockFile
    <> posixUnlockFile
    <> posixOpendir
    <> posixDirentBuf
    <> posixReaddir
    <> posixFreeDirent
    <> posixDName

posixFstatGetters :: AsteriusModule
posixFstatGetters =
  mempty
    { functionMap =
        SM.fromList
          [ ( k,
              Function
                { functionType =
                    FunctionType
                      { paramTypes = [I32],
                        returnTypes = [t]
                      },
                  varTypes = [],
                  body = f $ loadI32 GetLocal {index = 0, valueType = I32} v
                }
            )
            | (k, v, t, f) <-
                [ ("__hscore_st_mtime", offset_stat_mtime, I32, id),
                  ("__hscore_st_size", offset_stat_size, I64, extendUInt32),
                  ("__hscore_st_mode", offset_stat_mode, I32, id),
                  ("__hscore_st_dev", offset_stat_dev, I64, extendUInt32),
                  ("__hscore_st_ino", offset_stat_ino, I64, extendUInt32)
                ]
          ]
    }

posixModeGetters :: AsteriusModule
posixModeGetters =
  mempty
    { functionMap =
        SM.fromList
          [ ( k,
              Function
                { functionType =
                    FunctionType
                      { paramTypes = [I32],
                        returnTypes = [I32]
                      },
                  varTypes = [],
                  body =
                      ( GetLocal {index = 0, valueType = I32}
                          `andInt32` constI32 0o0170000
                      )
                        `eqInt32` constI32 v
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
        SM.fromList
          [ ( k,
              Function
                { functionType =
                    FunctionType
                      { paramTypes = [],
                        returnTypes = [I32]
                      },
                  varTypes = [],
                  body = constI32 v
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
  unsafePerformIO $
    allocaBytes sizeof_stat $ \p -> do
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
  setReturnTypes [I32]
  _ <- params [I32, I64, I64, I32]
  emit $ constI32 0
posixUnlockFile = runEDSL "unlockFile" $ do
  setReturnTypes [I32]
  _ <- params [I32]
  emit $ constI32 0

{-# NOINLINE unixUnitId #-}
unixUnitId :: BS.ByteString
unixUnitId = "unix"

posixOpendir :: AsteriusModule
posixOpendir =
  runEDSL
    ( mkEntitySymbol $
        "ghczuwrapperZC0ZC"
          <> unixUnitId
          <> "ZCSystemziPosixziDirectoryZCopendir"
    )
    $ do
      setReturnTypes [I32]
      p <- param I32
      call'
          "__asterius_posix_opendir"
          [p]
          I32
        >>= emit

posixDirentBuf :: AsteriusModule
posixDirentBuf =
  mempty
    { staticsMap =
        SM.singleton
          "__asterius_posix_dirent_buf"
          AsteriusStatics
            { asteriusStatics = [Serialized $ BS.pack $ replicate 4096 0]
            }
    }

posixReaddir :: AsteriusModule
posixReaddir = runEDSL "__hscore_readdir" $ do
  setReturnTypes [I32]
  [dirPtr, pDirEnt] <- params [I32, I32]
  call'
      "__asterius_posix_readdir"
      [dirPtr, symbol "__asterius_posix_dirent_buf"]
      I32
    >>= storeI32 pDirEnt 0
  emit $ constI32 0

posixFreeDirent :: AsteriusModule
posixFreeDirent = runEDSL "__hscore_free_dirent" $ do
  _ <- param I32
  pure ()

posixDName :: AsteriusModule
posixDName = runEDSL "__hscore_d_name" $ do
  setReturnTypes [I32]
  _ <- param I32
  emit $ symbol "__asterius_posix_dirent_buf"
