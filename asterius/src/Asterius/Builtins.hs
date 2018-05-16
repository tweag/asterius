{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , getDefaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , rtsAsteriusFunctionTypeMap
  , rtsAsteriusGlobalMap
  ) where

import Asterius.BuildInfo
import Asterius.Containers
import Asterius.Internals
import Asterius.Types
import qualified Data.ByteString.Short as SBS
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
  , threadStateSize :: Int
  }

getDefaultBuiltinsOptions :: IO BuiltinsOptions
getDefaultBuiltinsOptions =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
  GHC.runGhc (Just ghcLibDir) $ do
    _ <- GHC.getSessionDynFlags >>= GHC.setSessionDynFlags
    dflags <- GHC.getSessionDynFlags
    pure BuiltinsOptions {dflags = dflags, threadStateSize = 65536}

rtsAsteriusModuleSymbol :: AsteriusModuleSymbol
rtsAsteriusModuleSymbol =
  AsteriusModuleSymbol
    { unitId = SBS.toShort $ GHC.fs_bs $ GHC.unitIdFS GHC.rtsUnitId
    , moduleName = ["Asterius"]
    }

rtsAsteriusModule :: BuiltinsOptions -> AsteriusModule
rtsAsteriusModule opts =
  mempty
    { staticsMap = []
    , functionMap =
        [ ("rts_lock", rtsLockFunction opts)
        , ("rts_evalIO", rtsEvalIOFunction opts)
        , ("scheduleWaitThread", scheduleWaitThreadFunction opts)
        , ("appendToRunQueue", appendToRunQueueFunction opts)
        , ("setTSOLink", setTSOLinkFunction opts)
        , ("setTSOPrev", setTSOPrevFunction opts)
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
        ]
    }

rtsAsteriusFunctionTypeMap :: HashMap SBS.ShortByteString FunctionType
rtsAsteriusFunctionTypeMap =
  [ ("I64()", FunctionType {returnType = I64, paramTypes = []})
  , ("I64(I64,I64)", FunctionType {returnType = I64, paramTypes = [I64, I64]})
  , ( "I64(I64,I64,I64)"
    , FunctionType {returnType = I64, paramTypes = [I64, I64, I64]})
  , ("I64(I32)", FunctionType {returnType = I64, paramTypes = [I32]})
  , ("I64(I64)", FunctionType {returnType = I64, paramTypes = [I64]})
  , ("I64(I32,I64)", FunctionType {returnType = I64, paramTypes = [I32, I64]})
  , ("None(I64,I64)", FunctionType {returnType = None, paramTypes = [I64, I64]})
  , ( "None(I64,I64,I64)"
    , FunctionType {returnType = None, paramTypes = [I64, I64, I64]})
  ]

rtsAsteriusGlobalMap :: HashMap SBS.ShortByteString Global
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
      , "BaseReg"
      ]
  ]
  where
    rn :: String -> Int -> SBS.ShortByteString
    rn p i = fromString $ p <> show i
    w = Global {valueType = I64, mutable = True, initValue = ConstI64 0}
    f = Global {valueType = F32, mutable = True, initValue = ConstF32 0}
    d = Global {valueType = F64, mutable = True, initValue = ConstF64 0}

rtsLockFunction, rtsEvalIOFunction, scheduleWaitThreadFunction, appendToRunQueueFunction, setTSOLinkFunction, setTSOPrevFunction, createThreadFunction, createGenThreadFunction, createIOThreadFunction, createStrictIOThreadFunction, allocateFunction, allocateMightFailFunction, allocatePinnedFunction, allocBlockFunction, allocBlockLockFunction, allocBlockOnNodeFunction, allocBlockOnNodeLockFunction, allocGroupFunction, allocGroupLockFunction, allocGroupOnNodeFunction, allocGroupOnNodeLockFunction, newCAFFunction, stgRunFunction, stgReturnFunction ::
     BuiltinsOptions -> Function
rtsLockFunction _ =
  Function {functionTypeName = "I64()", varTypes = [], body = Unreachable}

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
                            [ getFieldWord cap 0
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
    cap = getLocalWord 0
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
              [ SetLocal {index = 3, value = getFieldWord pcap 0}
              , SetLocal
                  { index = 4
                  , value = getFieldWord cap offset_Capability_running_task
                  }
              , setFieldWord tso offset_StgTSO_bound $
                getFieldWord task offset_Task_incall
              , setFieldWord tso offset_StgTSO_cap cap
              , SetLocal
                  {index = 5, value = getFieldWord task offset_Task_incall}
              , setFieldWord incall offset_InCall_tso tso
              , setFieldWord incall offset_InCall_ret ret
              , setFieldWord32
                  incall
                  offset_InCall_rstat
                  (ConstI32 $ fromIntegral scheduler_NoStatus)
              , Call
                  { target = "appendToRunQueue"
                  , operands = [cap, tso]
                  , valueType = None
                  }
              ]
          , valueType = None
          }
    }
  where
    tso = getLocalWord 0
    ret = getLocalWord 1
    pcap = getLocalWord 2
    cap = getLocalWord 3
    task = getLocalWord 4
    incall = getLocalWord 5

appendToRunQueueFunction _ =
  Function
    { functionTypeName = "None(I64,I64)"
    , varTypes = []
    , body =
        Block
          { name = ""
          , bodys =
              [ If
                  { condition =
                      Binary
                        { binaryOp = EqInt64
                        , operand0 =
                            getFieldWord cap offset_Capability_run_queue_hd
                        , operand1 =
                            Unresolved
                              {unresolvedSymbol = "stg_END_TSO_QUEUE_closure"}
                        }
                  , ifTrue =
                      Block
                        { name = ""
                        , bodys =
                            [ setFieldWord
                                cap
                                offset_Capability_run_queue_hd
                                tso
                            , setFieldWord
                                tso
                                (offset_StgTSO_block_info +
                                 offset_StgTSOBlockInfo_prev)
                                Unresolved
                                  { unresolvedSymbol =
                                      "stg_END_TSO_QUEUE_closure"
                                  }
                            ]
                        , valueType = None
                        }
                  , ifFalse =
                      Block
                        { name = ""
                        , bodys =
                            [ Call
                                { target = "setTSOLink"
                                , operands =
                                    [ cap
                                    , getFieldWord
                                        cap
                                        offset_Capability_run_queue_tl
                                    , tso
                                    ]
                                , valueType = None
                                }
                            , Call
                                { target = "setTSOPrev"
                                , operands =
                                    [ cap
                                    , tso
                                    , getFieldWord
                                        cap
                                        offset_Capability_run_queue_tl
                                    ]
                                , valueType = None
                                }
                            ]
                        , valueType = None
                        }
                  }
              , setFieldWord cap offset_Capability_run_queue_tl tso
              , setFieldWord32
                  cap
                  offset_Capability_n_run_queue
                  Binary
                    { binaryOp = AddInt32
                    , operand0 =
                        getFieldWord32 cap offset_Capability_n_run_queue
                    , operand1 = ConstI32 1
                    }
              ]
          , valueType = None
          }
    }
  where
    cap = getLocalWord 0
    tso = getLocalWord 1

setTSOLinkFunction _ =
  Function
    { functionTypeName = "None(I64,I64,I64)"
    , varTypes = []
    , body =
        Block
          { name = ""
          , bodys =
              [ setFieldWord32 tso offset_StgTSO_dirty (ConstI32 1)
              , setFieldWord tso offset_StgTSO__link target
              ]
          , valueType = None
          }
    }
  where
    tso = getLocalWord 1
    target = getLocalWord 2

setTSOPrevFunction _ =
  Function
    { functionTypeName = "None(I64,I64,I64)"
    , varTypes = []
    , body =
        Block
          { name = ""
          , bodys =
              [ setFieldWord32 tso offset_StgTSO_dirty (ConstI32 1)
              , setFieldWord
                  tso
                  (offset_StgTSO_block_info + offset_StgTSOBlockInfo_prev)
                  target
              ]
          , valueType = None
          }
    }
  where
    tso = getLocalWord 1
    target = getLocalWord 2

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
                            ConstI64 $
                            fromIntegral $
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
                                    ConstI64 $
                                    fromIntegral $
                                    roundup_bytes_to_words sizeof_StgStopFrame
                                }
                        }
                  }
              , setFieldWord stack_p offset_StgStack_sp sp
              , setFieldWord32 stack_p offset_StgStack_dirty (ConstI32 1)
              , setFieldWord
                  tso_p
                  0
                  Unresolved {unresolvedSymbol = "stg_TSO_info"}
              , setFieldWord16 tso_p offset_StgTSO_what_next $
                ConstI32 $ fromIntegral next_ThreadRunGHC
              , setFieldWord16 tso_p offset_StgTSO_why_blocked $
                ConstI32 $ fromIntegral blocked_NotBlocked
              , setFieldWord
                  tso_p
                  (offset_StgTSO_block_info + offset_StgTSOBlockInfo_closure)
                  Unresolved {unresolvedSymbol = "stg_END_TSO_QUEUE_closure"}
              , setFieldWord
                  tso_p
                  offset_StgTSO_blocked_exceptions
                  Unresolved {unresolvedSymbol = "stg_END_TSO_QUEUE_closure"}
              , setFieldWord
                  tso_p
                  offset_StgTSO_bq
                  Unresolved {unresolvedSymbol = "stg_END_TSO_QUEUE_closure"}
              , setFieldWord32 tso_p offset_StgTSO_flags $ ConstI32 0
              , setFieldWord32 tso_p offset_StgTSO_dirty $ ConstI32 1
              , setFieldWord
                  tso_p
                  offset_StgTSO__link
                  Unresolved {unresolvedSymbol = "stg_END_TSO_QUEUE_closure"}
              , setFieldWord32 tso_p offset_StgTSO_saved_errno $ ConstI32 0
              , setFieldWord tso_p offset_StgTSO_bound $ ConstI64 0
              , setFieldWord tso_p offset_StgTSO_cap cap
              , setFieldWord tso_p offset_StgTSO_stackobj stack_p
              , setFieldWord32 tso_p offset_StgTSO_tot_stack_size $
                wrapI64 stack_size_w
              , setFieldWord tso_p offset_StgTSO_alloc_limit $ ConstI64 0
              , setFieldWord
                  tso_p
                  offset_StgTSO_trec
                  Unresolved {unresolvedSymbol = "stg_NO_TREC_closure"}
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
    cap = getLocalWord 0
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
              | (i, maybe_closure) <- zip [0 ..] closures
              ] <>
              [tso_p]
          , valueType = I64
          }
    }
  where
    cap = getLocalWord 0
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
                  , ifTrue = Unreachable
                  , ifFalse = Null
                  }
              , SetLocal
                  { index = 3
                  , value = UnresolvedGetGlobal {unresolvedGlobalReg = Hp}
                  }
              , UnresolvedSetGlobal {unresolvedGlobalReg = Hp, value = new_hp}
              , setFieldWord
                  (getFieldWord
                     UnresolvedGetGlobal {unresolvedGlobalReg = BaseReg}
                     offset_StgRegTable_rCurrentAlloc)
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
    , varTypes = [I64, I64]
    , body =
        Block
          { name = ""
          , bodys =
              [ SetLocal {index = 2, value = blocks_to_mblocks blocks_n}
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
                                        [ wrapI64
                                            Binary
                                              { binaryOp = MulInt64
                                              , operand0 = mblocks_n
                                              , operand1 =
                                                  constInt $
                                                  mblock_size `div` wasmPageSize
                                              }
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
                wrapI64
                  Binary
                    { binaryOp = AddInt64
                    , operand0 = constInt blocks_per_mblock
                    , operand1 =
                        Binary
                          { binaryOp = MulInt64
                          , operand0 =
                              Binary
                                { binaryOp = SubInt64
                                , operand0 = mblocks_n
                                , operand1 = ConstI64 1
                                }
                          , operand1 =
                              ConstI64 $
                              fromIntegral $ mblock_size `div` block_size
                          }
                    }
              , fieldOff mblocks_p offset_first_bdescr
              ]
          , valueType = I64
          }
    }
  where
    first_block_p = fieldOff mblocks_p offset_first_block
    blocks_n = getLocalWord 0
    mblocks_p = getLocalWord 1
    mblocks_n = getLocalWord 2
    mblock_round_up p =
      Binary
        { binaryOp = AndInt64
        , operand0 = fieldOff p $ mblock_size - 1
        , operand1 = constInt $ complement mblock_mask
        }
    blocks_to_mblocks n =
      Binary
        { binaryOp = AddInt64
        , operand0 = ConstI64 1
        , operand1 =
            Binary
              { binaryOp = DivSInt64
              , operand0 =
                  mblock_round_up
                    Binary
                      { binaryOp = MulInt64
                      , operand0 =
                          Binary
                            { binaryOp = SubInt64
                            , operand0 = n
                            , operand1 = constInt blocks_per_mblock
                            }
                      , operand1 = constInt block_size
                      }
              , operand1 = constInt mblock_size
              }
        }

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
    cap = fieldOff reg (-offset_Capability_r)
    orig_info = getFieldWord caf 0
    bh = getLocalWord 2

stgRunFunction _ =
  Function
    { functionTypeName = "None(I64,I64)"
    , varTypes = []
    , body =
        Block
          { name = ""
          , bodys =
              V.fromList $
              [ UnresolvedSetGlobal
                  {unresolvedGlobalReg = BaseReg, value = basereg}
              ] <>
              [ UnresolvedSetGlobal
                {unresolvedGlobalReg = gr, value = getFieldWord basereg o}
              | (gr, o) <- volatile_global_regs
              ] <>
              [ Loop
                  { name = loop_lbl
                  , body =
                      If
                        { condition =
                            Binary
                              { binaryOp = NeInt64
                              , operand0 = f
                              , operand1 = ConstI64 0
                              }
                        , ifTrue =
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
                        , ifFalse = Null
                        }
                  }
              ] <>
              [ setFieldWord
                basereg
                o
                UnresolvedGetGlobal {unresolvedGlobalReg = gr}
              | (gr, o) <- volatile_global_regs
              ]
          , valueType = None
          }
    }
  where
    loop_lbl = "StgRun_loop"
    f = getLocalWord 0
    basereg = getLocalWord 1
    volatile_global_regs =
      [ (Sp, offset_StgRegTable_rSp)
      , (SpLim, offset_StgRegTable_rSpLim)
      , (Hp, offset_StgRegTable_rHp)
      , (HpLim, offset_StgRegTable_rHpLim)
      , (CurrentTSO, offset_StgRegTable_rCurrentTSO)
      , (CurrentNursery, offset_StgRegTable_rCurrentNursery)
      ]

stgReturnFunction _ =
  Function {functionTypeName = "I64()", varTypes = [], body = ConstI64 0}

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

setFieldWord16 :: Expression -> Int -> Expression -> Expression
setFieldWord16 p o = storeWord16 (wrapI64 $ fieldOff p o)

storeWord16 :: Expression -> Expression -> Expression
storeWord16 p w =
  Store {bytes = 2, offset = 0, align = 0, ptr = p, value = w, valueType = I32}

wrapI64 :: Expression -> Expression
wrapI64 w = Unary {unaryOp = WrapInt64, operand0 = w}

words2Bytes :: Expression -> Expression
words2Bytes w =
  Binary {binaryOp = MulInt64, operand0 = w, operand1 = ConstI64 8}

constInt :: Int -> Expression
constInt = ConstI64 . fromIntegral

getLocalWord :: BinaryenIndex -> Expression
getLocalWord i = GetLocal {index = i, valueType = I64}

saveSp :: BinaryenIndex -> Expression -> Expression
saveSp sp_i tso_p =
  SetLocal {index = sp_i, value = fieldOff tso_p offset_StgTSO_StgStack}
