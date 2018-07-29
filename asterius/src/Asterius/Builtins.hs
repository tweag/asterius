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
import Asterius.EDSL
import Asterius.Internals
import Asterius.Types
import Asterius.TypesConv
import qualified Data.ByteString.Short as SBS
import Data.Foldable
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
  runEDSL $ do
    call "init_rts_asterius" []
    call "rts_evalIO" [mainCapability, symbol "Main_main_closure", constI64 0]

initRtsAsteriusFunction BuiltinsOptions {..} =
  runEDSL $ do
    bd <- call' "allocGroup" [constI64 nurseryGroups] I64
    putLVal hp $ loadI64 bd offset_bdescr_start
    putLVal hpLim $
      getLVal hp `addInt64`
      (extendUInt32 (loadI32 bd offset_bdescr_blocks) `mulInt64`
       constI64 block_size)
    putLVal cccs (constI64 0)
    putLVal currentNursery bd
    storeI64 (getLVal baseReg) offset_StgRegTable_rCurrentAlloc bd
    task <-
      call'
        "allocate"
        [mainCapability, constI64 $ roundup_bytes_to_words sizeof_Task]
        I64
    incall <-
      call'
        "allocate"
        [mainCapability, constI64 $ roundup_bytes_to_words sizeof_InCall]
        I64
    storeI64 mainCapability offset_Capability_running_task task
    storeI64 task offset_Task_cap mainCapability
    storeI64 task offset_Task_incall incall
    storeI64 incall offset_InCall_task task

rtsEvalIOFunction BuiltinsOptions {..} =
  runEDSL $ do
    [cap, p, ret] <- params [I64, I64, I64]
    tso <-
      call'
        "createStrictIOThread"
        [cap, constI64 $ roundup_bytes_to_words threadStateSize, p]
        I64
    call "scheduleWaitThread" [tso, ret, cap]

scheduleWaitThreadFunction _ =
  runEDSL $ do
    [tso, ret, cap] <- params [I64, I64, I64]
    task <- i64Local $ loadI64 cap offset_Capability_running_task
    storeI64 tso offset_StgTSO_bound $ loadI64 task offset_Task_incall
    storeI64 tso offset_StgTSO_cap cap
    incall <- i64Local $ loadI64 task offset_Task_incall
    storeI64 incall offset_InCall_tso tso
    storeI64 incall offset_InCall_ret ret
    storeI64 cap (offset_Capability_r + offset_StgRegTable_rCurrentTSO) tso
    storeI32 cap offset_Capability_interrupt (constI32 0)
    _ <-
      call'
        "StgRun"
        [ symbol "stg_returnToStackTop"
        , cap `addInt64` constI64 offset_Capability_r
        ]
        I64
    storeI64 incall offset_InCall_ret $
      loadI64 (loadI64 tso $ offset_StgTSO_StgStack + offset_StgStack_sp) 8

createThreadFunction _ =
  runEDSL $ do
    setReturnType I64
    [cap, alloc_words] <- params [I64, I64]
    tso_p <- call' "allocate" [cap, alloc_words] I64
    putLVal currentTSO tso_p
    stack_p <- i64Local $ tso_p `addInt64` constI64 offset_StgTSO_StgStack
    storeI64 stack_p 0 $ symbol "stg_STACK_info"
    stack_size_w <-
      i64Local $
      alloc_words `subInt64`
      constI64 ((offset_StgTSO_StgStack + offset_StgStack_stack) `div` 8)
    storeI32 stack_p offset_StgStack_stack_size $ wrapInt64 stack_size_w
    putLVal sp $
      (stack_p `addInt64` constI64 offset_StgStack_stack) `addInt64`
      (stack_size_w `subInt64`
       constI64 (roundup_bytes_to_words sizeof_StgStopFrame) `mulInt64`
       constI64 8)
    storeI64 stack_p offset_StgStack_sp (getLVal sp)
    storeI64 tso_p offset_StgTSO_cap cap
    storeI64 tso_p offset_StgTSO_stackobj stack_p
    storeI64 tso_p offset_StgTSO_alloc_limit (constI64 0)
    storeI64 (getLVal sp) 0 $ symbol "stg_stop_thread_info"
    emit tso_p

createThreadHelperFunction ::
     BuiltinsOptions -> [Maybe AsteriusEntitySymbol] -> AsteriusFunction
createThreadHelperFunction _ closures =
  runEDSL $ do
    setReturnType I64
    [cap, stack_size_w, target_closure] <- params [I64, I64, I64]
    tso_p <- call' "createThread" [cap, stack_size_w] I64
    stack_p <- i64Local $ tso_p `addInt64` constI64 offset_StgTSO_StgStack
    putLVal sp $
      loadI64 stack_p offset_StgStack_sp `addInt64`
      constI64 (-8 * length closures)
    storeI64 stack_p offset_StgStack_sp $ getLVal sp
    for_ (zip [0 ..] (reverse closures)) $ \(i, maybe_closure) ->
      storeI64 (getLVal sp) (i * 8) $
      case maybe_closure of
        Just closure -> symbol closure
        _ -> target_closure
    emit tso_p

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
  runEDSL $ do
    setReturnType I64
    [_, n] <- params [I64, I64]
    new_hp <- i64Local $ getLVal hp `addInt64` (n `mulInt64` constI64 8)
    if'
      (new_hp `gtUInt64` getLVal hpLim)
      (emit $ marshalErrorCode errHeapOverflow None)
      mempty
    old_hp <- i64Local $ getLVal hp
    putLVal hp new_hp
    storeI64
      (loadI64 (getLVal baseReg) offset_StgRegTable_rCurrentAlloc)
      offset_bdescr_free
      new_hp
    emit old_hp

allocateMightFailFunction _ =
  runEDSL $ do
    setReturnType I64
    xs <- params [I64, I64]
    r <- call' "allocate" xs I64
    emit r

allocatePinnedFunction = allocateMightFailFunction

allocBlockFunction _ =
  runEDSL $ do
    setReturnType I64
    r <- call' "allocGroup" [constI64 1] I64
    emit r

allocBlockLockFunction _ =
  runEDSL $ do
    setReturnType I64
    r <- call' "allocBlock" [] I64
    emit r

allocBlockOnNodeFunction _ =
  runEDSL $ do
    setReturnType I64
    _ <- param I32
    r <- call' "allocBlock" [] I64
    emit r

allocBlockOnNodeLockFunction _ =
  runEDSL $ do
    setReturnType I64
    node <- param I32
    r <- call' "allocBlockOnNode" [node] I64
    emit r

allocGroupFunction _ =
  runEDSL $ do
    setReturnType I64
    blocks_n <- param I64
    if'
      (blocks_n `gtUInt64` constI64 (1024 * blocks_per_mblock))
      (emit $ marshalErrorCode errMegaBlockGroup None)
      mempty
    mblocks_p <-
      i64Local $
      extendUInt32
        (growMemory (constI32 (1024 * (mblock_size `div` wasmPageSize)))) `mulInt64`
      constI64 wasmPageSize
    first_block_p <- i64Local $ mblocks_p `addInt64` constI64 offset_first_block
    storeI64 mblocks_p (offset_first_bdescr + offset_bdescr_start) first_block_p
    storeI64 mblocks_p (offset_first_bdescr + offset_bdescr_free) first_block_p
    storeI32 mblocks_p (offset_first_bdescr + offset_bdescr_blocks) $
      constI32 $ blocks_per_mblock + ((1023 * mblock_size) `div` block_size)
    emit $ mblocks_p `addInt64` constI64 offset_first_bdescr

allocGroupLockFunction _ =
  runEDSL $ do
    setReturnType I64
    x <- param I64
    r <- call' "allocGroup" [x] I64
    emit r

allocGroupOnNodeFunction _ =
  runEDSL $ do
    setReturnType I64
    [_, x] <- params [I32, I64]
    r <- call' "allocGroup" [x] I64
    emit r

allocGroupOnNodeLockFunction _ =
  runEDSL $ do
    setReturnType I64
    xs <- params [I32, I64]
    r <- call' "allocGroupOnNode" xs I64
    emit r

freeFunction _ =
  runEDSL $ do
    _ <- param I64
    emit $ marshalErrorCode errUnimplemented None

newCAFFunction _ =
  runEDSL $ do
    setReturnType I64
    [reg, caf] <- params [I64, I64]
    orig_info <- i64Local $ loadI64 caf 0
    storeI64 caf offset_StgIndStatic_saved_info orig_info
    bh <-
      call'
        "allocate"
        [mainCapability, constI64 $ roundup_bytes_to_words sizeof_StgInd]
        I64
    storeI64 bh 0 $ symbol "stg_CAF_BLACKHOLE_info"
    storeI64 bh offset_StgInd_indirectee $
      loadI64 reg offset_StgRegTable_rCurrentTSO
    storeI64 caf offset_StgIndStatic_indirectee bh
    storeI64 caf 0 $ symbol "stg_IND_STATIC_info"
    emit bh

stgRunFunction BuiltinsOptions {..} =
  runEDSL $ do
    setReturnType I64
    f <- mutParam I64
    _ <- param I64
    loop $ \loop_lbl ->
      if' (eqZInt64 (getLVal f)) mempty $ do
        f' <-
          callIndirect'
            (getLVal f `subInt64` constI64 1)
            []
            (FunctionType I64 [])
        putLVal f f'
        break' loop_lbl Null
    emit $ getLVal r1

stgReturnFunction _ =
  runEDSL $ do
    setReturnType I64
    emit $ constI64 0

printI64Function _ =
  runEDSL $ do
    x <- param I64
    callImport "printI64" (V.toList (cutI64 x))

printF32Function _ =
  runEDSL $ do
    x <- param F32
    callImport "printF32" [x]

printF64Function _ =
  runEDSL $ do
    x <- param F64
    callImport "printF64" [x]

getI32GlobalRegFunction ::
     BuiltinsOptions -> UnresolvedGlobalReg -> AsteriusFunction
getI32GlobalRegFunction _ gr =
  runEDSL $ do
    setReturnType I32
    emit $ wrapInt64 $ getLVal $ global gr

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

loadWord :: Expression -> Expression
loadWord p =
  Load
    {signed = False, bytes = 8, offset = 0, align = 0, valueType = I64, ptr = p}

wrapI64 :: Expression -> Expression
wrapI64 w = Unary {unaryOp = WrapInt64, operand0 = w}

constInt :: Int -> Expression
constInt = ConstI64 . fromIntegral

getLocalWord :: BinaryenIndex -> Expression
getLocalWord i = GetLocal {index = i, valueType = I64}

mainCap :: Expression
mainCap = Unresolved {unresolvedSymbol = "MainCapability"}

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
