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
import Asterius.Internals.Session
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Foldable
import qualified DynFlags as GHC
import qualified FastString as GHC
import Foreign
import qualified Module as GHC
import qualified Packages as GHC
import System.IO.Unsafe
import System.Posix.Internals

posixImports :: [FunctionImport]
posixImports =
  [ FunctionImport
      { internalName = "__asterius_posix_get_errno",
        externalModuleName = "posix",
        externalBaseName = "get_errno",
        functionType = FunctionType {paramTypes = [], returnTypes = [F64]}
      },
    FunctionImport
      { internalName = "__asterius_posix_set_errno",
        externalModuleName = "posix",
        externalBaseName = "set_errno",
        functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      },
    FunctionImport
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
      { internalName = "__asterius_posix_close",
        externalModuleName = "posix",
        externalBaseName = "close",
        functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      },
    FunctionImport
      { internalName = "__asterius_posix_stat",
        externalModuleName = "posix",
        externalBaseName = "stat",
        functionType =
          FunctionType
            { paramTypes = [F64, F64],
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
      },
    FunctionImport
      { internalName = "__asterius_posix_opendir",
        externalModuleName = "posix",
        externalBaseName = "opendir",
        functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      },
    FunctionImport
      { internalName = "__asterius_posix_readdir",
        externalModuleName = "posix",
        externalBaseName = "readdir",
        functionType =
          FunctionType
            { paramTypes = [F64, F64],
              returnTypes = [F64]
            }
      },
    FunctionImport
      { internalName = "__asterius_posix_closedir",
        externalModuleName = "posix",
        externalBaseName = "closedir",
        functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      },
    FunctionImport
      { internalName = "__asterius_posix_getenv",
        externalModuleName = "posix",
        externalBaseName = "getenv",
        functionType =
          FunctionType
            { paramTypes = [F64, F64],
              returnTypes = [F64]
            }
      },
    FunctionImport
      { internalName = "__asterius_posix_access",
        externalModuleName = "posix",
        externalBaseName = "access",
        functionType =
          FunctionType
            { paramTypes = [F64, F64],
              returnTypes = [F64]
            }
      },
    FunctionImport
      { internalName = "__asterius_posix_getcwd",
        externalModuleName = "posix",
        externalBaseName = "getcwd",
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
    <> posixClose
    <> posixStat
    <> posixFstat
    <> posixFstatGetters
    <> posixModeGetters
    <> posixConstants
    <> posixLockFile
    <> posixUnlockFile
    <> posixOpendir
    <> posixGetErrno
    <> posixSetErrno
    <> posixDirentBuf
    <> posixReaddir
    <> posixFreeDirent
    <> posixDName
    <> posixClosedir
    <> posixGetenvBuf
    <> posixGetenv
    <> posixAccess
    <> posixGetcwd

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

posixClose :: AsteriusModule
posixClose = runEDSL "close" $ do
  setReturnTypes [I64]
  fd <- param I64
  truncSFloat64ToInt64
    <$> callImport' "__asterius_posix_close" [convertSInt64ToFloat64 fd] F64
    >>= emit

posixStat :: AsteriusModule
posixStat = runEDSL "__hscore_stat" $ do
  setReturnTypes [I64]
  args <- params [I64, I64]
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_posix_stat"
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
        SM.fromList
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
        SM.fromList
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
        SM.fromList
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

{-# NOINLINE unixUnitId #-}
unixUnitId :: BS.ByteString
unixUnitId = unsafePerformIO $ fakeSession $ do
  dflags <- GHC.getDynFlags
  let Just comp_id = GHC.lookupPackageName dflags (GHC.PackageName "unix")
      GHC.InstalledUnitId inst_unit_id =
        GHC.componentIdToInstalledUnitId comp_id
  liftIO $ evaluate $ GHC.fastZStringToByteString $
    GHC.zEncodeFS
      inst_unit_id

posixOpendir :: AsteriusModule
posixOpendir =
  runEDSL
    ( mkEntitySymbol $ "ghczuwrapperZC0ZC"
        <> unixUnitId
        <> "ZCSystemziPosixziDirectoryZCopendir"
    )
    $ do
      setReturnTypes [I64]
      p <- param I64
      truncSFloat64ToInt64
        <$> callImport'
          "__asterius_posix_opendir"
          [convertSInt64ToFloat64 p]
          F64
        >>= emit

posixGetErrno :: AsteriusModule
posixGetErrno = runEDSL "__hscore_get_errno" $ do
  setReturnTypes [I64]
  truncSFloat64ToInt64
    <$> callImport' "__asterius_posix_get_errno" [] F64
    >>= emit

posixSetErrno :: AsteriusModule
posixSetErrno = runEDSL "__hscore_set_errno" $ do
  e <- param I64
  callImport "__asterius_posix_set_errno" [convertSInt64ToFloat64 e]

posixDirentBuf :: AsteriusModule
posixDirentBuf =
  mempty
    { staticsMap =
        SM.singleton
          "__asterius_posix_dirent_buf"
          AsteriusStatics
            { staticsType = Bytes,
              asteriusStatics = [Uninitialized 4096]
            }
    }

posixReaddir :: AsteriusModule
posixReaddir = runEDSL "__hscore_readdir" $ do
  setReturnTypes [I64]
  [dirPtr, pDirEnt] <- params [I64, I64]
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_posix_readdir"
      ( map
          convertSInt64ToFloat64
          [dirPtr, symbol "__asterius_posix_dirent_buf"]
      )
      F64
    >>= storeI64 pDirEnt 0
  emit $ constI64 0

posixFreeDirent :: AsteriusModule
posixFreeDirent = runEDSL "__hscore_free_dirent" $ do
  _ <- param I64
  pure ()

posixDName :: AsteriusModule
posixDName = runEDSL "__hscore_d_name" $ do
  setReturnTypes [I64]
  _ <- param I64
  emit $ symbol "__asterius_posix_dirent_buf"

posixClosedir :: AsteriusModule
posixClosedir = runEDSL "closedir" $ do
  setReturnTypes [I64]
  p <- param I64
  truncSFloat64ToInt64
    <$> callImport' "__asterius_posix_closedir" [convertSInt64ToFloat64 p] F64
    >>= emit

posixGetenvBuf :: AsteriusModule
posixGetenvBuf =
  mempty
    { staticsMap =
        SM.singleton
          "__asterius_posix_getenv_buf"
          AsteriusStatics
            { staticsType = Bytes,
              asteriusStatics = [Uninitialized 32768]
            }
    }

posixGetenv :: AsteriusModule
posixGetenv = runEDSL "getenv" $ do
  setReturnTypes [I64]
  p <- param I64
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_posix_getenv"
      (map convertSInt64ToFloat64 [p, symbol "__asterius_posix_getenv_buf"])
      F64
    >>= emit

posixAccess :: AsteriusModule
posixAccess = runEDSL "access" $ do
  setReturnTypes [I64]
  args <- params [I64, I64]
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_posix_access"
      (map convertSInt64ToFloat64 args)
      F64
    >>= emit

posixGetcwd :: AsteriusModule
posixGetcwd = runEDSL "getcwd" $ do
  setReturnTypes [I64]
  args <- params [I64, I64]
  truncSFloat64ToInt64
    <$> callImport'
      "__asterius_posix_getcwd"
      (map convertSInt64ToFloat64 args)
      F64
    >>= emit
