{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , getDefaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , rtsAsteriusFunctionImports
  , rtsAsteriusFunctionExports
  , rtsAsteriusFunctionTypeMap
  , rtsAsteriusGlobalMap
  , marshalErrorCode
  , errGCEnter1
  , errGCFun
  , errBarf
  , errStgGC
  , errUnreachableBlock
  , errHeapOverflow
  , errMegaBlockGroup
  , wasmPageSize
  ) where

import Asterius.BuildInfo
import Asterius.Internals
import Asterius.Types
import qualified Data.ByteString.Short as SBS
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Vector as V
import Foreign
import qualified GHC
import GHC.Exts
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
        [ ( "MainCapability"
          , AsteriusStatics
              { asteriusStatics =
                  [Uninitialized $ 8 * roundup_bytes_to_words sizeof_Capability]
              })
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
        , ("newCAF", newCAFFunction opts)
        , ("StgRun", stgRunFunction opts)
        , ("StgReturn", stgReturnFunction opts)
        , ("print_int", printIntFunction opts)
        , ("_get_Sp", getI32GlobalRegFunction opts Sp)
        , ("_get_SpLim", getI32GlobalRegFunction opts SpLim)
        , ("_get_Hp", getI32GlobalRegFunction opts Hp)
        , ("_get_HpLim", getI32GlobalRegFunction opts HpLim)
        ]
    }

rtsAsteriusFunctionImports :: V.Vector FunctionImport
rtsAsteriusFunctionImports =
  [ FunctionImport
      { internalName = "printI32"
      , externalModuleName = "rts"
      , externalBaseName = "print"
      , functionTypeName = "None(I32)"
      }
  , FunctionImport
      { internalName = "errorI32"
      , externalModuleName = "rts"
      , externalBaseName = "panic"
      , functionTypeName = "None(I32)"
      }
  , FunctionImport
      { internalName = "traceCmm"
      , externalModuleName = "rts"
      , externalBaseName = "traceCmm"
      , functionTypeName = "None(I32)"
      }
  , FunctionImport
      { internalName = "traceCmmBlock"
      , externalModuleName = "rts"
      , externalBaseName = "traceCmmBlock"
      , functionTypeName = "None(I32,I32)"
      }
  , FunctionImport
      { internalName = "traceCmmSetLocal"
      , externalModuleName = "rts"
      , externalBaseName = "traceCmmSetLocal"
      , functionTypeName = "None(I32,I32,I32)"
      }
  ]

rtsAsteriusFunctionExports :: V.Vector FunctionExport
rtsAsteriusFunctionExports =
  V.fromList
    [ FunctionExport {internalName = f, externalName = f}
    | f <- ["main", "_get_Sp", "_get_SpLim", "_get_Hp", "_get_HpLim"]
    ]

rtsAsteriusFunctionTypeMap :: HM.HashMap SBS.ShortByteString FunctionType
rtsAsteriusFunctionTypeMap =
  [ ("I64()", FunctionType {returnType = I64, paramTypes = []})
  , ("I64(I64,I64)", FunctionType {returnType = I64, paramTypes = [I64, I64]})
  , ( "I64(I64,I64,I64)"
    , FunctionType {returnType = I64, paramTypes = [I64, I64, I64]})
  , ("I64(I32)", FunctionType {returnType = I64, paramTypes = [I32]})
  , ("I64(I64)", FunctionType {returnType = I64, paramTypes = [I64]})
  , ("I64(I32,I64)", FunctionType {returnType = I64, paramTypes = [I32, I64]})
  , ("I32()", FunctionType {returnType = I32, paramTypes = []})
  , ("None()", FunctionType {returnType = None, paramTypes = []})
  , ("None(I32)", FunctionType {returnType = None, paramTypes = [I32]})
  , ("None(I32,I32)", FunctionType {returnType = None, paramTypes = [I32, I32]})
  , ( "None(I32,I32,I32)"
    , FunctionType {returnType = None, paramTypes = [I32, I32, I32]})
  , ("None(I64)", FunctionType {returnType = None, paramTypes = [I64]})
  , ("None(I64,I64)", FunctionType {returnType = None, paramTypes = [I64, I64]})
  , ( "None(I64,I64,I64)"
    , FunctionType {returnType = None, paramTypes = [I64, I64, I64]})
  ]

rtsAsteriusGlobalMap :: HM.HashMap SBS.ShortByteString Global
rtsAsteriusGlobalMap =
  fromList $
  [(rn "R" i, w) | i <- [1 .. 10]] <> [(rn "F" i, f) | i <- [1 .. 6]] <>
  [(rn "D" i, d) | i <- [1 .. 6]] <>
  [ (k, w)
  | k <-
      [ "L1"
      , "Sp"
      , "SpLim"
      , "Hp"
      , "HpLim"
      , "CurrentTSO"
      , "CurrentNursery"
      , "HpAlloc"
      ]
  ] <>
  [ ( "BaseReg"
    , Global
        { valueType = I64
        , mutable = False
        , initValue =
            UnresolvedOff
              { unresolvedSymbol = "MainCapability"
              , offset' = offset_Capability_r
              }
        })
  ]
  where
    rn :: String -> Int -> SBS.ShortByteString
    rn p i = fromString $ p <> show i
    w = Global {valueType = I64, mutable = True, initValue = ConstI64 0}
    f = Global {valueType = F32, mutable = True, initValue = ConstF32 0}
    d = Global {valueType = F64, mutable = True, initValue = ConstF64 0}

{-# INLINEABLE marshalErrorCode #-}
marshalErrorCode :: Int32 -> ValueType -> Expression
marshalErrorCode err vt =
  Block
    { name = ""
    , bodys =
        [ CallImport
            {target' = "errorI32", operands = [ConstI32 err], valueType = None}
        , Unreachable
        ]
    , valueType = vt
    }

errGCEnter1, errGCFun, errBarf, errStgGC, errUnreachableBlock, errHeapOverflow, errMegaBlockGroup ::
     Int32
errGCEnter1 = 1

errGCFun = 2

errBarf = 3

errStgGC = 4

errUnreachableBlock = 5

errHeapOverflow = 6

errMegaBlockGroup = 7

mainFunction, initRtsAsteriusFunction, rtsEvalIOFunction, scheduleWaitThreadFunction, createThreadFunction, createGenThreadFunction, createIOThreadFunction, createStrictIOThreadFunction, allocateFunction, allocateMightFailFunction, allocatePinnedFunction, allocBlockFunction, allocBlockLockFunction, allocBlockOnNodeFunction, allocBlockOnNodeLockFunction, allocGroupFunction, allocGroupLockFunction, allocGroupOnNodeFunction, allocGroupOnNodeLockFunction, newCAFFunction, stgRunFunction, stgReturnFunction, printIntFunction ::
     BuiltinsOptions -> Function
mainFunction BuiltinsOptions {..} =
  Function
    { functionTypeName = "None()"
    , varTypes = []
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
  Function
    { functionTypeName = "None()"
    , varTypes = [I64, I64, I64]
    , body =
        Block
          { name = ""
          , bodys =
              [ SetLocal
                  { index = 0
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
                                    { unaryOp = ExtendSInt32
                                    , operand0 =
                                        getFieldWord32 bd offset_bdescr_blocks
                                    }
                              , operand1 = constInt block_size
                              }
                        }
                  }
              , setFieldWord baseReg offset_StgRegTable_rCurrentNursery bd
              , setFieldWord baseReg offset_StgRegTable_rCurrentAlloc bd
              , SetLocal
                  { index = 1
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
              , SetLocal
                  { index = 2
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
    bd = getLocalWord 0
    task = getLocalWord 1
    incall = getLocalWord 2

rtsEvalIOFunction BuiltinsOptions {..} =
  Function
    { functionTypeName = "None(I64,I64,I64)"
    , varTypes = [I64]
    , body =
        Block
          { name = ""
          , bodys =
              [ SetLocal
                  { index = 3
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
    tso = getLocalWord 3

scheduleWaitThreadFunction _ =
  Function
    { functionTypeName = "None(I64,I64,I64)"
    , varTypes = [I64, I64, I64]
    , body =
        Block
          { name = ""
          , bodys =
              [ SetLocal
                  { index = 3
                  , value = getFieldWord cap offset_Capability_running_task
                  }
              , setFieldWord tso offset_StgTSO_bound $
                getFieldWord task offset_Task_incall
              , setFieldWord tso offset_StgTSO_cap cap
              , SetLocal
                  {index = 4, value = getFieldWord task offset_Task_incall}
              , setFieldWord incall offset_InCall_tso tso
              , setFieldWord incall offset_InCall_ret ret
              , setFieldWord
                  cap
                  (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
                  tso
              , setFieldWord32 cap offset_Capability_interrupt (ConstI32 0)
              , SetLocal
                  { index = 5
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
    task = getLocalWord 3
    incall = getLocalWord 4

createThreadFunction _ =
  Function
    { functionTypeName = "I64(I64,I64)"
    , varTypes = [I64, I64, I64, I64]
    , body =
        Block
          { name = ""
          , bodys =
              [ SetLocal
                  { index = 2
                  , value =
                      Call
                        { target = "allocate"
                        , operands = [getLocalWord 0, alloc_words]
                        , valueType = I64
                        }
                  }
              , saveSp 3 tso_p
              , setFieldWord
                  stack_p
                  0
                  Unresolved {unresolvedSymbol = "stg_STACK_info"}
              , SetLocal
                  { index = 4
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
              , SetLocal
                  { index = 5
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
    tso_p = getLocalWord 2
    stack_p = getLocalWord 3
    stack_size_w = getLocalWord 4
    sp = getLocalWord 5

createThreadHelperFunction ::
     BuiltinsOptions -> [Maybe AsteriusEntitySymbol] -> Function
createThreadHelperFunction _ closures =
  Function
    { functionTypeName = "I64(I64,I64,I64)"
    , varTypes = [I64, I64, I64]
    , body =
        Block
          { name = ""
          , bodys =
              V.fromList $
              [ SetLocal
                  { index = 3
                  , value =
                      Call
                        { target = "createThread"
                        , operands = [cap, stack_size_w]
                        , valueType = I64
                        }
                  }
              , saveSp 4 tso_p
              , SetLocal
                  { index = 5
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
    tso_p = getLocalWord 3
    stack_p = getLocalWord 4
    sp = getLocalWord 5

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
  Function
    { functionTypeName = "I64(I64,I64)"
    , varTypes = [I64, I64]
    , body =
        Block
          { name = ""
          , bodys =
              [ SetLocal
                  { index = 2
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
              , SetLocal
                  { index = 3
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
    new_hp = getLocalWord 2
    old_hp = getLocalWord 3

allocateMightFailFunction _ =
  Function
    { functionTypeName = "I64(I64,I64)"
    , varTypes = []
    , body =
        Call
          { target = "allocate"
          , operands = [getLocalWord 0, getLocalWord 1]
          , valueType = I64
          }
    }

allocatePinnedFunction _ =
  Function
    { functionTypeName = "I64(I64,I64)"
    , varTypes = []
    , body =
        Call
          { target = "allocate"
          , operands = [getLocalWord 0, getLocalWord 1]
          , valueType = I64
          }
    }

allocBlockFunction _ =
  Function
    { functionTypeName = "I64()"
    , varTypes = []
    , body =
        Call {target = "allocGroup", operands = [ConstI64 1], valueType = I64}
    }

allocBlockLockFunction _ =
  Function
    { functionTypeName = "I64()"
    , varTypes = []
    , body = Call {target = "allocBlock", operands = [], valueType = I64}
    }

allocBlockOnNodeFunction _ =
  Function
    { functionTypeName = "I64(I32)"
    , varTypes = []
    , body = Call {target = "allocBlock", operands = [], valueType = I64}
    }

allocBlockOnNodeLockFunction _ =
  Function
    { functionTypeName = "I64(I32)"
    , varTypes = []
    , body =
        Call
          { target = "allocBlockOnNode"
          , operands = [GetLocal {index = 0, valueType = I32}]
          , valueType = I64
          }
    }

allocGroupFunction _ =
  Function
    { functionTypeName = "I64(I64)"
    , varTypes = [I64]
    , body =
        Block
          { name = ""
          , bodys =
              [ If
                  { condition =
                      Binary
                        { binaryOp = GtSInt64
                        , operand0 = blocks_n
                        , operand1 = constInt blocks_per_mblock
                        }
                  , ifTrue = marshalErrorCode errMegaBlockGroup None
                  , ifFalse = Null
                  }
              , SetLocal
                  { index = 1
                  , value =
                      Binary
                        { binaryOp = MulInt64
                        , operand0 =
                            Unary
                              { unaryOp = ExtendSInt32
                              , operand0 =
                                  Host
                                    { hostOp = GrowMemory
                                    , name = ""
                                    , operands =
                                        [ constInt32 $
                                          mblock_size `div` wasmPageSize
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
                constInt32 blocks_per_mblock
              , fieldOff mblocks_p offset_first_bdescr
              ]
          , valueType = I64
          }
    }
  where
    first_block_p = fieldOff mblocks_p offset_first_block
    blocks_n = getLocalWord 0
    mblocks_p = getLocalWord 1

allocGroupLockFunction _ =
  Function
    { functionTypeName = "I64(I64)"
    , varTypes = []
    , body =
        Call
          {target = "allocGroup", operands = [getLocalWord 0], valueType = I64}
    }

allocGroupOnNodeFunction _ =
  Function
    { functionTypeName = "I64(I32,I64)"
    , varTypes = []
    , body =
        Call
          {target = "allocGroup", operands = [getLocalWord 1], valueType = I64}
    }

allocGroupOnNodeLockFunction _ =
  Function
    { functionTypeName = "I64(I32,I64)"
    , varTypes = []
    , body =
        Call
          { target = "allocGroupOnNode"
          , operands = [GetLocal {index = 0, valueType = I32}, getLocalWord 1]
          , valueType = I64
          }
    }

newCAFFunction _ =
  Function
    { functionTypeName = "I64(I64,I64)"
    , varTypes = [I64]
    , body =
        Block
          { name = ""
          , bodys =
              [ setFieldWord caf offset_StgIndStatic_saved_info orig_info
              , SetLocal
                  { index = 2
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
    bh = getLocalWord 2

stgRunFunction BuiltinsOptions {..} =
  Function
    { functionTypeName = "I64(I64,I64)"
    , varTypes = []
    , body =
        Block
          { name = ""
          , bodys =
              V.fromList $
              [ UnresolvedSetGlobal
                {unresolvedGlobalReg = gr, value = getFieldWord baseReg o}
              | (gr, o) <- volatile_global_regs
              ] <>
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
              ] <>
              [UnresolvedGetGlobal {unresolvedGlobalReg = VanillaReg 1}]
          , valueType = I64
          }
    }
  where
    loop_lbl = "StgRun_loop"
    f = getLocalWord 0
    volatile_global_regs =
      [ (CurrentTSO, offset_StgRegTable_rCurrentTSO)
      , (CurrentNursery, offset_StgRegTable_rCurrentNursery)
      ]

stgReturnFunction _ =
  Function {functionTypeName = "I64()", varTypes = [], body = ConstI64 0}

printIntFunction _ =
  Function
    { functionTypeName = "None(I64)"
    , varTypes = []
    , body =
        CallImport
          {target' = "printI32", operands = [wrapI64 x], valueType = None}
    }
  where
    x = getLocalWord 0

getI32GlobalRegFunction :: BuiltinsOptions -> UnresolvedGlobalReg -> Function
getI32GlobalRegFunction _ gr =
  Function
    { functionTypeName = "I32()"
    , varTypes = []
    , body = wrapI64 UnresolvedGetGlobal {unresolvedGlobalReg = gr}
    }

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

getLocalWord :: BinaryenIndex -> Expression
getLocalWord i = GetLocal {index = i, valueType = I64}

saveSp :: BinaryenIndex -> Expression -> Expression
saveSp sp_i tso_p =
  SetLocal {index = sp_i, value = fieldOff tso_p offset_StgTSO_StgStack}

mainCap :: Expression
mainCap = Unresolved {unresolvedSymbol = "MainCapability"}

baseReg :: Expression
baseReg = UnresolvedGetGlobal {unresolvedGlobalReg = BaseReg}

offset_StgTSO_StgStack :: Int
offset_StgTSO_StgStack = 8 * roundup_bytes_to_words sizeof_StgTSO
