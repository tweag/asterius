{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , getDefaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , rtsAsteriusFunctionImports
  , rtsAsteriusFunctionExports
  , marshalErrorCode
  , errGCEnter1
  , errGCFun
  , errBarf
  , errStgGC
  , errUnreachableBlock
  , errHeapOverflow
  , errMegaBlockGroup
  , errUnimplemented
  , errAtomics
  , errSetBaseReg
  , errBrokenFunction
  , wasmPageSize
  , cutI64
  , generateWasmFunctionTypeName
  ) where

import Asterius.BuildInfo
import Asterius.Internals
import Asterius.Types
import qualified Data.ByteString.Short as SBS
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import Foreign
import qualified GHC
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)

wasmPageSize :: Int
wasmPageSize = 65536

data BuiltinsOptions = BuiltinsOptions
  { dflags :: GHC.DynFlags
  , nurseryGroups, threadStateSize :: Int
  , tracing :: Bool
  }

getDefaultBuiltinsOptions :: IO BuiltinsOptions
getDefaultBuiltinsOptions =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
  GHC.runGhc (Just ghcLibDir) $ do
    _ <- GHC.getSessionDynFlags >>= GHC.setSessionDynFlags
    dflags <- GHC.getSessionDynFlags
    pure
      BuiltinsOptions
        { dflags = dflags
        , nurseryGroups = blocks_per_mblock
        , threadStateSize = 65536
        , tracing = False
        }

rtsAsteriusModuleSymbol :: AsteriusModuleSymbol
rtsAsteriusModuleSymbol =
  AsteriusModuleSymbol
    { unitId = SBS.toShort $ GHC.fs_bs $ GHC.unitIdFS GHC.rtsUnitId
    , moduleName = ["Asterius"]
    }

rtsAsteriusModule :: BuiltinsOptions -> AsteriusModule
rtsAsteriusModule opts =
  mempty
    { staticsMap =
        [ ("g0", AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        , ( "blocked_queue_hd"
          , AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        , ( "blocked_queue_tl"
          , AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        , ( "enabled_capabilities"
          , AsteriusStatics {asteriusStatics = [Uninitialized 4]})
        , ( "large_alloc_lim"
          , AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        , ( "MainCapability"
          , AsteriusStatics
              { asteriusStatics =
                  [ Serialized $
                    SBS.pack $
                    replicate (8 * roundup_bytes_to_words sizeof_Capability) 0
                  ]
              })
        , ( "n_capabilities"
          , AsteriusStatics {asteriusStatics = [Uninitialized 4]})
        , ( "rts_stop_on_exception"
          , AsteriusStatics {asteriusStatics = [Uninitialized 4]})
        , ( "RtsFlags"
          , AsteriusStatics
              { asteriusStatics =
                  [Uninitialized (8 * roundup_bytes_to_words sizeof_RTS_FLAGS)]
              })
        , ( "stable_ptr_table"
          , AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        ]
    , functionMap =
        [ ("main", mainFunction opts)
        , ("init_rts_asterius", initRtsAsteriusFunction opts)
        , ("rts_evalIO", rtsEvalIOFunction opts)
        , ("scheduleWaitThread", scheduleWaitThreadFunction opts)
        , ("createThread", createThreadFunction opts)
        , ("createGenThread", createGenThreadFunction opts)
        , ("createIOThread", createIOThreadFunction opts)
        , ("createStrictIOThread", createStrictIOThreadFunction opts)
        , ("allocate", allocateFunction opts)
        , ("allocateMightFail", allocateMightFailFunction opts)
        , ("allocatePinned", allocatePinnedFunction opts)
        , ("allocBlock", allocBlockFunction opts)
        , ("allocBlock_lock", allocBlockLockFunction opts)
        , ("allocBlockOnNode", allocBlockOnNodeFunction opts)
        , ("allocBlockOnNode_lock", allocBlockOnNodeLockFunction opts)
        , ("allocGroup", allocGroupFunction opts)
        , ("allocGroup_lock", allocGroupLockFunction opts)
        , ("allocGroupOnNode", allocGroupOnNodeFunction opts)
        , ("allocGroupOnNode_lock", allocGroupOnNodeLockFunction opts)
        , ("free", freeFunction opts)
        , ("newCAF", newCAFFunction opts)
        , ("StgRun", stgRunFunction opts)
        , ("StgReturn", stgReturnFunction opts)
        , ("print_i64", printI64Function opts)
        , ("print_f32", printF32Function opts)
        , ("print_f64", printF64Function opts)
        , ("__asterius_Load_Sp", getI32GlobalRegFunction opts Sp)
        , ("__asterius_Load_SpLim", getI32GlobalRegFunction opts SpLim)
        , ("__asterius_Load_Hp", getI32GlobalRegFunction opts Hp)
        , ("__asterius_Load_HpLim", getI32GlobalRegFunction opts HpLim)
        , ("__asterius_memory_trap", memoryTrapFunction opts)
        , ("__asterius_Load_I8", loadWrapperFunction opts 1 I32)
        , ("__asterius_Load_I16", loadWrapperFunction opts 2 I32)
        , ("__asterius_Load_I32", loadWrapperFunction opts 4 I32)
        , ("__asterius_Load_I64", loadWrapperFunction opts 8 I64)
        , ("__asterius_Load_F32", loadWrapperFunction opts 4 F32)
        , ("__asterius_Load_F64", loadWrapperFunction opts 8 F64)
        , ("__asterius_Store_I8", storeWrapperFunction opts 1 I32)
        , ("__asterius_Store_I16", storeWrapperFunction opts 2 I32)
        , ("__asterius_Store_I32", storeWrapperFunction opts 4 I32)
        , ("__asterius_Store_I64", storeWrapperFunction opts 8 I64)
        , ("__asterius_Store_F32", storeWrapperFunction opts 4 F32)
        , ("__asterius_Store_F64", storeWrapperFunction opts 8 F64)
        ]
    }

generateWasmFunctionTypeName :: FunctionType -> SBS.ShortByteString
generateWasmFunctionTypeName FunctionType {..} =
  showSBS returnType <> "(" <>
  mconcat (intersperse "," [showSBS t | t <- V.toList paramTypes]) <>
  ")"

rtsAsteriusFunctionImports :: Bool -> [AsteriusFunctionImport]
rtsAsteriusFunctionImports debug =
  [ AsteriusFunctionImport
    { internalName = "__asterius_" <> op <> "_" <> showSBS ft
    , externalModuleName = "Math"
    , externalBaseName = op
    , functionType = FunctionType {returnType = ft, paramTypes = [ft]}
    }
  | ft <- [F32, F64]
  , op <-
      [ "sin"
      , "cos"
      , "tan"
      , "sinh"
      , "cosh"
      , "tanh"
      , "asin"
      , "acos"
      , "atan"
      , "log"
      , "exp"
      ]
  ] <>
  [ AsteriusFunctionImport
    { internalName = "__asterius_" <> op <> "_" <> showSBS ft
    , externalModuleName = "Math"
    , externalBaseName = op
    , functionType = FunctionType {returnType = ft, paramTypes = [ft, ft]}
    }
  | ft <- [F32, F64]
  , op <- ["pow"]
  ] <>
  [ AsteriusFunctionImport
      { internalName = "printI64"
      , externalModuleName = "rts"
      , externalBaseName = "printI64"
      , functionType = FunctionType {returnType = None, paramTypes = [I32, I32]}
      }
  , AsteriusFunctionImport
      { internalName = "printF32"
      , externalModuleName = "rts"
      , externalBaseName = "print"
      , functionType = FunctionType {returnType = None, paramTypes = [F32]}
      }
  , AsteriusFunctionImport
      { internalName = "printF64"
      , externalModuleName = "rts"
      , externalBaseName = "print"
      , functionType = FunctionType {returnType = None, paramTypes = [F64]}
      }
  , AsteriusFunctionImport
      { internalName = "__asterius_errorI32"
      , externalModuleName = "rts"
      , externalBaseName = "panic"
      , functionType = FunctionType {returnType = None, paramTypes = [I32]}
      }
  ] <>
  (if debug
     then [ AsteriusFunctionImport
              { internalName = "__asterius_traceCmm"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_traceCmm"
              , functionType =
                  FunctionType {returnType = None, paramTypes = [I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_traceCmmBlock"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_traceCmmBlock"
              , functionType =
                  FunctionType {returnType = None, paramTypes = [I32, I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_traceCmmSetLocal"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_traceCmmSetLocal"
              , functionType =
                  FunctionType
                    {returnType = None, paramTypes = [I32, I32, I32, I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_memory_trap_trigger"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_memory_trap_trigger"
              , functionType =
                  FunctionType {returnType = None, paramTypes = [I32, I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_load_i64"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_load_i64"
              , functionType =
                  FunctionType
                    {returnType = None, paramTypes = [I32, I32, I32, I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_store_i64"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_store_i64"
              , functionType =
                  FunctionType
                    {returnType = None, paramTypes = [I32, I32, I32, I32]}
              }
          ] <>
          concat
            [ [ AsteriusFunctionImport
                  { internalName = "__asterius_load_" <> k
                  , externalModuleName = "rts"
                  , externalBaseName = "__asterius_load_" <> k
                  , functionType =
                      FunctionType
                        {returnType = None, paramTypes = [I32, I32, t]}
                  }
              , AsteriusFunctionImport
                  { internalName = "__asterius_store_" <> k
                  , externalModuleName = "rts"
                  , externalBaseName = "__asterius_store_" <> k
                  , functionType =
                      FunctionType
                        {returnType = None, paramTypes = [I32, I32, t]}
                  }
            ]
            | (k, t) <-
                [ ("i8", I32)
                , ("i16", I32)
                , ("i32", I32)
                , ("f32", F32)
                , ("f64", F64)
                ]
            ]
     else [])

rtsAsteriusFunctionExports :: Bool -> V.Vector FunctionExport
rtsAsteriusFunctionExports debug =
  V.fromList
    [ FunctionExport {internalName = f, externalName = f}
    | f <-
        if debug
          then [ "main"
               , "__asterius_Load_Sp"
               , "__asterius_Load_SpLim"
               , "__asterius_Load_Hp"
               , "__asterius_Load_HpLim"
               ]
          else ["main"]
    ]

{-# INLINEABLE marshalErrorCode #-}
marshalErrorCode :: Int32 -> ValueType -> Expression
marshalErrorCode err vt =
  Block
    { name = ""
    , bodys =
        [ CallImport
            { target' = "__asterius_errorI32"
            , operands = [ConstI32 err]
            , valueType = None
            }
        , Unreachable
        ]
    , valueType = vt
    }

errGCEnter1, errGCFun, errBarf, errStgGC, errUnreachableBlock, errHeapOverflow, errMegaBlockGroup, errUnimplemented, errAtomics, errSetBaseReg, errBrokenFunction ::
     Int32
errGCEnter1 = 1

errGCFun = 2

errBarf = 3

errStgGC = 4

errUnreachableBlock = 5

errHeapOverflow = 6

errMegaBlockGroup = 7

errUnimplemented = 8

errAtomics = 9

errSetBaseReg = 10

errBrokenFunction = 11

mainFunction, initRtsAsteriusFunction, rtsEvalIOFunction, scheduleWaitThreadFunction, createThreadFunction, createGenThreadFunction, createIOThreadFunction, createStrictIOThreadFunction, allocateFunction, allocateMightFailFunction, allocatePinnedFunction, allocBlockFunction, allocBlockLockFunction, allocBlockOnNodeFunction, allocBlockOnNodeLockFunction, allocGroupFunction, allocGroupLockFunction, allocGroupOnNodeFunction, allocGroupOnNodeLockFunction, freeFunction, newCAFFunction, stgRunFunction, stgReturnFunction, printI64Function, printF32Function, printF64Function, memoryTrapFunction ::
     BuiltinsOptions -> AsteriusFunction
mainFunction BuiltinsOptions {..} =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = []}
    , body =
        Block
          { name = ""
          , bodys =
              [ Call
                  { target = "init_rts_asterius"
                  , operands = []
                  , valueType = None
                  }
              , Call
                  { target = "rts_evalIO"
                  , operands =
                      [ mainCap
                      , Unresolved {unresolvedSymbol = "Main_main_closure"}
                      , constInt 0
                      ]
                  , valueType = None
                  }
              ]
          , valueType = None
          }
    }

initRtsAsteriusFunction BuiltinsOptions {..} =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = []}
    , body =
        Block
          { name = ""
          , bodys =
              [ UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 0 I64
                  , value =
                      Call
                        { target = "allocGroup"
                        , operands = [constInt nurseryGroups]
                        , valueType = I64
                        }
                  }
              , UnresolvedSetGlobal
                  { unresolvedGlobalReg = Hp
                  , value = getFieldWord bd offset_bdescr_start
                  }
              , UnresolvedSetGlobal
                  { unresolvedGlobalReg = HpLim
                  , value =
                      Binary
                        { binaryOp = AddInt64
                        , operand0 =
                            UnresolvedGetGlobal {unresolvedGlobalReg = Hp}
                        , operand1 =
                            Binary
                              { binaryOp = MulInt64
                              , operand0 =
                                  Unary
                                    { unaryOp = ExtendUInt32
                                    , operand0 =
                                        getFieldWord32 bd offset_bdescr_blocks
                                    }
                              , operand1 = constInt block_size
                              }
                        }
                  }
              , setFieldWord baseReg offset_StgRegTable_rCCCS (ConstI64 0)
              , setFieldWord baseReg offset_StgRegTable_rCurrentNursery bd
              , setFieldWord baseReg offset_StgRegTable_rCurrentAlloc bd
              , UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 1 I64
                  , value =
                      Call
                        { target = "allocate"
                        , operands =
                            [ mainCap
                            , constInt $ roundup_bytes_to_words sizeof_Task
                            ]
                        , valueType = I64
                        }
                  }
              , UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 2 I64
                  , value =
                      Call
                        { target = "allocate"
                        , operands =
                            [ mainCap
                            , constInt $ roundup_bytes_to_words sizeof_InCall
                            ]
                        , valueType = I64
                        }
                  }
              , setFieldWord mainCap offset_Capability_running_task task
              , setFieldWord task offset_Task_cap mainCap
              , setFieldWord task offset_Task_incall incall
              , setFieldWord incall offset_InCall_task task
              ]
          , valueType = None
          }
    }
  where
    bd = getUnresolvedLocalWord 0
    task = getUnresolvedLocalWord 1
    incall = getUnresolvedLocalWord 2

rtsEvalIOFunction BuiltinsOptions {..} =
  AsteriusFunction
    { functionType =
        FunctionType {returnType = None, paramTypes = [I64, I64, I64]}
    , body =
        Block
          { name = ""
          , bodys =
              [ UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 3 I64
                  , value =
                      Call
                        { target = "createStrictIOThread"
                        , operands =
                            [ cap
                            , constInt $ roundup_bytes_to_words threadStateSize
                            , p
                            ]
                        , valueType = I64
                        }
                  }
              , Call
                  { target = "scheduleWaitThread"
                  , operands = [tso, ret, cap]
                  , valueType = None
                  }
              ]
          , valueType = None
          }
    }
  where
    cap = mainCap
    p = getLocalWord 1
    ret = getLocalWord 2
    tso = getUnresolvedLocalWord 3

scheduleWaitThreadFunction _ =
  AsteriusFunction
    { functionType =
        FunctionType {returnType = None, paramTypes = [I64, I64, I64]}
    , body =
        Block
          { name = ""
          , bodys =
              [ UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 3 I64
                  , value = getFieldWord cap offset_Capability_running_task
                  }
              , setFieldWord tso offset_StgTSO_bound $
                getFieldWord task offset_Task_incall
              , setFieldWord tso offset_StgTSO_cap cap
              , UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 4 I64
                  , value = getFieldWord task offset_Task_incall
                  }
              , setFieldWord incall offset_InCall_tso tso
              , setFieldWord incall offset_InCall_ret ret
              , setFieldWord
                  cap
                  (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
                  tso
              , setFieldWord32 cap offset_Capability_interrupt (ConstI32 0)
              , UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 5 I64
                  , value =
                      Call
                        { target = "StgRun"
                        , operands =
                            [ Unresolved
                                {unresolvedSymbol = "stg_returnToStackTop"}
                            , fieldOff cap offset_Capability_r
                            ]
                        , valueType = I64
                        }
                  }
              , setFieldWord incall offset_InCall_ret $
                getFieldWord
                  (getFieldWord tso $
                   offset_StgTSO_StgStack + offset_StgStack_sp)
                  8
              ]
          , valueType = None
          }
    }
  where
    tso = getLocalWord 0
    ret = getLocalWord 1
    cap = mainCap
    task = getUnresolvedLocalWord 3
    incall = getUnresolvedLocalWord 4

createThreadFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I64, I64]}
    , body =
        Block
          { name = ""
          , bodys =
              [ UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 2 I64
                  , value =
                      Call
                        { target = "allocate"
                        , operands = [getLocalWord 0, alloc_words]
                        , valueType = I64
                        }
                  }
              , UnresolvedSetGlobal
                  {unresolvedGlobalReg = CurrentTSO, value = tso_p}
              , saveUnresolvedSp 3 tso_p
              , setFieldWord
                  stack_p
                  0
                  Unresolved {unresolvedSymbol = "stg_STACK_info"}
              , UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 4 I64
                  , value =
                      Binary
                        { binaryOp = SubInt64
                        , operand0 = alloc_words
                        , operand1 =
                            constInt $
                            (offset_StgTSO_StgStack + offset_StgStack_stack) `div`
                            8
                        }
                  }
              , setFieldWord32
                  stack_p
                  offset_StgStack_stack_size
                  (wrapI64 stack_size_w)
              , UnresolvedSetGlobal
                  { unresolvedGlobalReg = Sp
                  , value =
                      Binary
                        { binaryOp = AddInt64
                        , operand0 = fieldOff stack_p offset_StgStack_stack
                        , operand1 =
                            words2Bytes
                              Binary
                                { binaryOp = SubInt64
                                , operand0 = stack_size_w
                                , operand1 =
                                    constInt $
                                    roundup_bytes_to_words sizeof_StgStopFrame
                                }
                        }
                  }
              , setFieldWord stack_p offset_StgStack_sp sp
              , setFieldWord tso_p offset_StgTSO_cap cap
              , setFieldWord tso_p offset_StgTSO_stackobj stack_p
              , setFieldWord tso_p offset_StgTSO_alloc_limit (ConstI64 0)
              , setFieldWord
                  sp
                  0
                  Unresolved {unresolvedSymbol = "stg_stop_thread_info"}
              , tso_p
              ]
          , valueType = I64
          }
    }
  where
    cap = mainCap
    alloc_words = getLocalWord 1
    tso_p = getUnresolvedLocalWord 2
    stack_p = getUnresolvedLocalWord 3
    stack_size_w = getUnresolvedLocalWord 4
    sp = UnresolvedGetGlobal {unresolvedGlobalReg = Sp}

createThreadHelperFunction ::
     BuiltinsOptions -> [Maybe AsteriusEntitySymbol] -> AsteriusFunction
createThreadHelperFunction _ closures =
  AsteriusFunction
    { functionType =
        FunctionType {returnType = I64, paramTypes = [I64, I64, I64]}
    , body =
        Block
          { name = ""
          , bodys =
              V.fromList $
              [ UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 3 I64
                  , value =
                      Call
                        { target = "createThread"
                        , operands = [cap, stack_size_w]
                        , valueType = I64
                        }
                  }
              , saveUnresolvedSp 4 tso_p
              , UnresolvedSetGlobal
                  { unresolvedGlobalReg = Sp
                  , value =
                      fieldOff
                        (getFieldWord stack_p offset_StgStack_sp)
                        (-8 * length closures)
                  }
              , setFieldWord stack_p offset_StgStack_sp sp
              ] <>
              [ setFieldWord
                sp
                (i * 8)
                (case maybe_closure of
                   Just closure -> Unresolved {unresolvedSymbol = closure}
                   _ -> target_closure)
              | (i, maybe_closure) <- zip [0 ..] (reverse closures)
              ] <>
              [tso_p]
          , valueType = I64
          }
    }
  where
    cap = mainCap
    stack_size_w = getLocalWord 1
    target_closure = getLocalWord 2
    tso_p = getUnresolvedLocalWord 3
    stack_p = getUnresolvedLocalWord 4
    sp = UnresolvedGetGlobal {unresolvedGlobalReg = Sp}

createGenThreadFunction opts =
  createThreadHelperFunction opts [Nothing, Just "stg_enter_info"]

createIOThreadFunction opts =
  createThreadHelperFunction
    opts
    [Just "stg_ap_v_info", Nothing, Just "stg_enter_info"]

createStrictIOThreadFunction opts =
  createThreadHelperFunction
    opts
    [ Just "stg_forceIO_info"
    , Just "stg_ap_v_info"
    , Nothing
    , Just "stg_enter_info"
    ]

allocateFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I64, I64]}
    , body =
        Block
          { name = ""
          , bodys =
              [ UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 2 I64
                  , value =
                      Binary
                        { binaryOp = AddInt64
                        , operand0 =
                            UnresolvedGetGlobal {unresolvedGlobalReg = Hp}
                        , operand1 = words2Bytes n
                        }
                  }
              , If
                  { condition =
                      Binary
                        { binaryOp = GtSInt64
                        , operand0 = new_hp
                        , operand1 =
                            UnresolvedGetGlobal {unresolvedGlobalReg = HpLim}
                        }
                  , ifTrue = marshalErrorCode errHeapOverflow None
                  , ifFalse = Null
                  }
              , UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 3 I64
                  , value = UnresolvedGetGlobal {unresolvedGlobalReg = Hp}
                  }
              , UnresolvedSetGlobal {unresolvedGlobalReg = Hp, value = new_hp}
              , setFieldWord
                  (getFieldWord baseReg offset_StgRegTable_rCurrentAlloc)
                  offset_bdescr_free
                  new_hp
              , old_hp
              ]
          , valueType = I64
          }
    }
  where
    n = getLocalWord 1
    new_hp = getUnresolvedLocalWord 2
    old_hp = getUnresolvedLocalWord 3

allocateMightFailFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I64, I64]}
    , body =
        Call
          { target = "allocate"
          , operands = [getLocalWord 0, getLocalWord 1]
          , valueType = I64
          }
    }

allocatePinnedFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I64, I64]}
    , body =
        Call
          { target = "allocate"
          , operands = [getLocalWord 0, getLocalWord 1]
          , valueType = I64
          }
    }

allocBlockFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = []}
    , body =
        Call {target = "allocGroup", operands = [ConstI64 1], valueType = I64}
    }

allocBlockLockFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = []}
    , body = Call {target = "allocBlock", operands = [], valueType = I64}
    }

allocBlockOnNodeFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I32]}
    , body = Call {target = "allocBlock", operands = [], valueType = I64}
    }

allocBlockOnNodeLockFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I32]}
    , body =
        Call
          { target = "allocBlockOnNode"
          , operands = [GetLocal {index = 0, valueType = I32}]
          , valueType = I64
          }
    }

allocGroupFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I64]}
    , body =
        Block
          { name = ""
          , bodys =
              [ If
                  { condition =
                      Binary
                        { binaryOp = GtSInt64
                        , operand0 = blocks_n
                        , operand1 = constInt $ 1024 * blocks_per_mblock
                        }
                  , ifTrue = marshalErrorCode errMegaBlockGroup None
                  , ifFalse = Null
                  }
              , UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 1 I64
                  , value =
                      Binary
                        { binaryOp = MulInt64
                        , operand0 =
                            Unary
                              { unaryOp = ExtendUInt32
                              , operand0 =
                                  Host
                                    { hostOp = GrowMemory
                                    , name = ""
                                    , operands =
                                        [ constInt32 $
                                          1024 *
                                          (mblock_size `div` wasmPageSize)
                                        ]
                                    }
                              }
                        , operand1 = constInt wasmPageSize
                        }
                  }
              , setFieldWord
                  mblocks_p
                  (offset_first_bdescr + offset_bdescr_start)
                  first_block_p
              , setFieldWord
                  mblocks_p
                  (offset_first_bdescr + offset_bdescr_free)
                  first_block_p
              , setFieldWord32
                  mblocks_p
                  (offset_first_bdescr + offset_bdescr_blocks) $
                constInt32 $
                blocks_per_mblock + ((1023 * mblock_size) `div` block_size)
              , fieldOff mblocks_p offset_first_bdescr
              ]
          , valueType = I64
          }
    }
  where
    first_block_p = fieldOff mblocks_p offset_first_block
    blocks_n = getLocalWord 0
    mblocks_p = getUnresolvedLocalWord 1

allocGroupLockFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I64]}
    , body =
        Call
          {target = "allocGroup", operands = [getLocalWord 0], valueType = I64}
    }

allocGroupOnNodeFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I32, I64]}
    , body =
        Call
          {target = "allocGroup", operands = [getLocalWord 1], valueType = I64}
    }

allocGroupOnNodeLockFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I32, I64]}
    , body =
        Call
          { target = "allocGroupOnNode"
          , operands = [GetLocal {index = 0, valueType = I32}, getLocalWord 1]
          , valueType = I64
          }
    }

freeFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = [I64]}
    , body = marshalErrorCode errUnimplemented None
    }

newCAFFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I64, I64]}
    , body =
        Block
          { name = ""
          , bodys =
              [ setFieldWord caf offset_StgIndStatic_saved_info orig_info
              , UnresolvedSetLocal
                  { unresolvedLocalReg = UniqueLocalReg 2 I64
                  , value =
                      Call
                        { target = "allocate"
                        , operands =
                            [ cap
                            , constInt $ roundup_bytes_to_words sizeof_StgInd
                            ]
                        , valueType = I64
                        }
                  }
              , setFieldWord
                  bh
                  0
                  Unresolved {unresolvedSymbol = "stg_CAF_BLACKHOLE_info"}
              , setFieldWord bh offset_StgInd_indirectee $
                getFieldWord reg offset_StgRegTable_rCurrentTSO
              , setFieldWord caf offset_StgIndStatic_indirectee bh
              , setFieldWord
                  caf
                  0
                  Unresolved {unresolvedSymbol = "stg_IND_STATIC_info"}
              , bh
              ]
          , valueType = I64
          }
    }
  where
    reg = getLocalWord 0
    caf = getLocalWord 1
    cap = mainCap
    orig_info = getFieldWord caf 0
    bh = getUnresolvedLocalWord 2

stgRunFunction BuiltinsOptions {..} =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = [I64, I64]}
    , body =
        Block
          { name = ""
          , bodys =
              [ Loop
                  { name = loop_lbl
                  , body =
                      If
                        { condition = Unary {unaryOp = EqZInt64, operand0 = f}
                        , ifTrue = Nop
                        , ifFalse =
                            Block
                              { name = ""
                              , bodys =
                                  [ SetLocal
                                      { index = 0
                                      , value =
                                          CallIndirect
                                            { indirectTarget =
                                                Unary
                                                  { unaryOp = WrapInt64
                                                  , operand0 =
                                                      Binary
                                                        { binaryOp = SubInt64
                                                        , operand0 =
                                                            GetLocal
                                                              { index = 0
                                                              , valueType = I64
                                                              }
                                                        , operand1 = ConstI64 1
                                                        }
                                                  }
                                            , operands = []
                                            , typeName = "I64()"
                                            }
                                      }
                                  , Break
                                      { name = loop_lbl
                                      , condition = Null
                                      , value = Null
                                      }
                                  ]
                              , valueType = None
                              }
                        }
                  }
              , UnresolvedGetGlobal {unresolvedGlobalReg = VanillaReg 1}
              ]
          , valueType = I64
          }
    }
  where
    loop_lbl = "StgRun_loop"
    f = getLocalWord 0

stgReturnFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = I64, paramTypes = []}
    , body = ConstI64 0
    }

printI64Function _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = [I64]}
    , body =
        CallImport {target' = "printI64", operands = cutI64 x, valueType = None}
    }
  where
    x = getLocalWord 0

printF32Function _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = [F32]}
    , body = CallImport {target' = "printF32", operands = [x], valueType = None}
    }
  where
    x = GetLocal {index = 0, valueType = F32}

printF64Function _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = [F64]}
    , body = CallImport {target' = "printF64", operands = [x], valueType = None}
    }
  where
    x = GetLocal {index = 0, valueType = F64}

getI32GlobalRegFunction ::
     BuiltinsOptions -> UnresolvedGlobalReg -> AsteriusFunction
getI32GlobalRegFunction _ gr =
  AsteriusFunction
    { functionType = FunctionType {returnType = I32, paramTypes = []}
    , body = wrapI64 UnresolvedGetGlobal {unresolvedGlobalReg = gr}
    }

memoryTrapFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = [I64]}
    , body =
        If
          { condition =
              V.foldl1' (Binary OrInt32) $
              V.fromList $
              [ guard_struct (ConstI64 0) 8 []
              , guard_struct
                  task_p
                  sizeof_Task
                  [offset_Task_cap, offset_Task_incall]
              , Binary
                  { binaryOp = AndInt32
                  , operand0 =
                      notExpr $ Unary {unaryOp = EqZInt64, operand0 = tso_p}
                  , operand1 =
                      guard_struct
                        tso_p
                        sizeof_StgTSO
                        [ offset_StgTSO_alloc_limit
                        , offset_StgTSO_bound
                        , offset_StgTSO_cap
                        , offset_StgTSO_stackobj
                        , offset_StgTSO_what_next
                        ]
                  }
              ] <>
              [ guard_struct Unresolved {unresolvedSymbol = sym} size []
              | (sym, size) <-
                  [ ("g0", 8)
                  , ("blocked_queue_hd", 8)
                  , ("blocked_queue_tl", 8)
                  , ("enabled_capabilities", 4)
                  , ("large_alloc_lim", 8)
                  , ("n_capabilities", 4)
                  , ("rts_stop_on_exception", 4)
                  , ("RtsFlags", 8 * roundup_bytes_to_words sizeof_RTS_FLAGS)
                  , ("stable_ptr_table", 8)
                  ]
              ]
          , ifTrue =
              Block
                { name = ""
                , bodys =
                    [ CallImport
                        { target' = "__asterius_memory_trap_trigger"
                        , operands = cutI64 p
                        , valueType = None
                        }
                    , Unreachable
                    ]
                , valueType = None
                }
          , ifFalse = Null
          }
    }
  where
    p = getLocalWord 0
    tso_p = UnresolvedGetGlobal {unresolvedGlobalReg = CurrentTSO}
    task_p = getFieldWord mainCap offset_Capability_running_task
    guard_struct struct_addr_expr struct_size allowed_field_offsets =
      Binary
        { binaryOp = AndInt32
        , operand0 =
            Binary
              { binaryOp = AndInt32
              , operand0 =
                  Binary
                    { binaryOp = GeUInt64
                    , operand0 = p
                    , operand1 = struct_addr_expr
                    }
              , operand1 =
                  Binary
                    { binaryOp = LtUInt64
                    , operand0 = p
                    , operand1 = struct_field_off struct_size
                    }
              }
        , operand1 =
            notExpr $
            V.foldl' (Binary OrInt32) (ConstI32 0) $
            V.fromList
              [ Binary
                { binaryOp = EqInt64
                , operand0 =
                    Binary
                      { binaryOp = SubInt64
                      , operand0 = p
                      , operand1 = struct_addr_expr
                      }
                , operand1 = constInt o
                }
              | o <- allowed_field_offsets
              ]
        }
      where
        struct_field_off o =
          Binary
            { binaryOp = AddInt64
            , operand0 = struct_addr_expr
            , operand1 = constInt o
            }

loadWrapperFunction, storeWrapperFunction ::
     BuiltinsOptions -> BinaryenIndex -> ValueType -> AsteriusFunction
loadWrapperFunction _ b vt =
  AsteriusFunction
    { functionType = FunctionType {returnType = vt, paramTypes = [I64]}
    , body =
        Block
          { name = ""
          , bodys =
              [ CallImport
                  { target' =
                      case (vt, b) of
                        (I32, 1) -> "__asterius_load_i8"
                        (I32, 2) -> "__asterius_load_i16"
                        (I32, 4) -> "__asterius_load_i32"
                        (I64, 8) -> "__asterius_load_i64"
                        (F32, 4) -> "__asterius_load_f32"
                        (F64, 8) -> "__asterius_load_f64"
                        _ ->
                          error $
                          "Unsupported ValueType/ByteLength: " <> show (vt, b)
                  , operands =
                      cutI64 p <>
                      (case vt of
                         I64 -> cutI64 v
                         _ -> [v])
                  , valueType = None
                  }
              , Call
                  { target = "__asterius_memory_trap"
                  , operands = [p]
                  , valueType = None
                  }
              , v
              ]
          , valueType = vt
          }
    }
  where
    p = getLocalWord 0
    v =
      Load
        { signed = False
        , bytes = b
        , offset = 0
        , align = 0
        , valueType = vt
        , ptr = wrapI64 p
        }

storeWrapperFunction _ b vt =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = [I64, vt]}
    , body =
        Block
          { name = ""
          , bodys =
              [ CallImport
                  { target' =
                      case (vt, b) of
                        (I32, 1) -> "__asterius_store_i8"
                        (I32, 2) -> "__asterius_store_i16"
                        (I32, 4) -> "__asterius_store_i32"
                        (I64, 8) -> "__asterius_store_i64"
                        (F32, 4) -> "__asterius_store_f32"
                        (F64, 8) -> "__asterius_store_f64"
                        _ ->
                          error $
                          "Unsupported ValueType/ByteLength: " <> show (vt, b)
                  , operands =
                      cutI64 p <>
                      (case vt of
                         I64 -> cutI64 v
                         _ -> [v])
                  , valueType = None
                  }
              , Call
                  { target = "__asterius_memory_trap"
                  , operands = [p]
                  , valueType = None
                  }
              , Store
                  { bytes = b
                  , offset = 0
                  , align = 0
                  , ptr = wrapI64 p
                  , value = v
                  , valueType = vt
                  }
              ]
          , valueType = None
          }
    }
  where
    p = getLocalWord 0
    v = GetLocal {index = 1, valueType = vt}

fieldOff :: Expression -> Int -> Expression
fieldOff p o
  | o == 0 = p
  | otherwise =
    Binary {binaryOp = AddInt64, operand0 = p, operand1 = constInt o}

getFieldWord :: Expression -> Int -> Expression
getFieldWord p o = loadWord (wrapI64 $ fieldOff p o)

getFieldWord32 :: Expression -> Int -> Expression
getFieldWord32 p o = loadWord32 (wrapI64 $ fieldOff p o)

setFieldWord :: Expression -> Int -> Expression -> Expression
setFieldWord p o = storeWord (wrapI64 $ fieldOff p o)

loadWord :: Expression -> Expression
loadWord p =
  Load
    {signed = False, bytes = 8, offset = 0, align = 0, valueType = I64, ptr = p}

loadWord32 :: Expression -> Expression
loadWord32 p =
  Load
    {signed = False, bytes = 4, offset = 0, align = 0, valueType = I32, ptr = p}

storeWord :: Expression -> Expression -> Expression
storeWord p w =
  Store {bytes = 8, offset = 0, align = 0, ptr = p, value = w, valueType = I64}

setFieldWord32 :: Expression -> Int -> Expression -> Expression
setFieldWord32 p o = storeWord32 (wrapI64 $ fieldOff p o)

storeWord32 :: Expression -> Expression -> Expression
storeWord32 p w =
  Store {bytes = 4, offset = 0, align = 0, ptr = p, value = w, valueType = I32}

wrapI64 :: Expression -> Expression
wrapI64 w = Unary {unaryOp = WrapInt64, operand0 = w}

words2Bytes :: Expression -> Expression
words2Bytes w =
  Binary {binaryOp = MulInt64, operand0 = w, operand1 = ConstI64 8}

constInt :: Int -> Expression
constInt = ConstI64 . fromIntegral

constInt32 :: Int -> Expression
constInt32 = ConstI32 . fromIntegral

getUnresolvedLocalWord :: Int -> Expression
getUnresolvedLocalWord i =
  UnresolvedGetLocal {unresolvedLocalReg = UniqueLocalReg i I64}

getLocalWord :: BinaryenIndex -> Expression
getLocalWord i = GetLocal {index = i, valueType = I64}

saveUnresolvedSp :: Int -> Expression -> Expression
saveUnresolvedSp sp_i tso_p =
  UnresolvedSetLocal
    { unresolvedLocalReg = UniqueLocalReg sp_i I64
    , value = fieldOff tso_p offset_StgTSO_StgStack
    }

mainCap :: Expression
mainCap = Unresolved {unresolvedSymbol = "MainCapability"}

baseReg :: Expression
baseReg = UnresolvedGetGlobal {unresolvedGlobalReg = BaseReg}

offset_StgTSO_StgStack :: Int
offset_StgTSO_StgStack = 8 * roundup_bytes_to_words sizeof_StgTSO

cutI64 :: Expression -> V.Vector Expression
cutI64 x =
  [ wrapI64 x
  , wrapI64 $
    Binary {binaryOp = ShrUInt64, operand0 = x, operand1 = ConstI64 32}
  ]

notExpr :: Expression -> Expression
notExpr = Binary XorInt32 (ConstI32 0xFFFFFFFF)
