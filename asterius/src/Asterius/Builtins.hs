{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , getDefaultBuiltinsOptions
  , unsafeDefaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , rtsFunctionImports
  , rtsAsteriusFunctionExports
  , emitErrorMessage
  , wasmPageSize
  , cutI64
  , generateWrapperFunction
  ) where

import Asterius.BuildInfo
import Asterius.EDSL
import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import Control.Monad (when)
import Data.Bits
import qualified Data.ByteString.Short as SBS
import Data.Foldable
import Data.List
import Data.Maybe
import Foreign (Word64, Word8)
import qualified GHC
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)
import System.IO.Unsafe

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
        , nurseryGroups = blocks_per_mblock * 1024
        , threadStateSize = 65536
        , tracing = False
        }

{-# NOINLINE unsafeDefaultBuiltinsOptions #-}
unsafeDefaultBuiltinsOptions :: BuiltinsOptions
unsafeDefaultBuiltinsOptions = unsafePerformIO getDefaultBuiltinsOptions

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
        [ ( "heap_overflow"
          , AsteriusStatics
              { isConstant = False
              , asteriusStatics = [Serialized (encodeStorable (0 :: Word8))]
              })
        , ( "MainCapability"
          , AsteriusStatics
              { isConstant = False
              , asteriusStatics =
                  [ Serialized $
                    SBS.pack $
                    replicate (8 * roundup_bytes_to_words sizeof_Capability) 0
                  ]
              })
        , ( "recent_activity"
          , AsteriusStatics
              { isConstant = False
              , asteriusStatics =
                  [ Serialized
                      (encodeStorable
                         (fromIntegral recent_ACTIVITY_YES :: Word64))
                  ]
              })
        , ( "sched_state"
          , AsteriusStatics
              { isConstant = False
              , asteriusStatics =
                  [ Serialized
                      (encodeStorable
                         (fromIntegral sched_SCHED_RUNNING :: Word64))
                  ]
              })
        ]
    , functionMap =
        [ ("main", mainFunction opts)
        , ("hs_init", hsInitFunction opts)
        , ("rts_apply", rtsApplyFunction opts)
        , ( "rts_apply_wrapper"
          , generateWrapperFunction "rts_apply" $ rtsApplyFunction opts)
        , ("rts_eval", rtsEvalFunction opts)
        , ( "rts_eval_wrapper"
          , generateWrapperFunction "rts_eval" $ rtsEvalFunction opts)
        , ("rts_evalIO", rtsEvalIOFunction opts)
        , ( "rts_evalIO_wrapper"
          , generateWrapperFunction "rts_evalIO" $ rtsEvalIOFunction opts)
        , ("rts_evalLazyIO", rtsEvalLazyIOFunction opts)
        , ( "rts_evalLazyIO_wrapper"
          , generateWrapperFunction "rts_evalLazyIO" $
            rtsEvalLazyIOFunction opts)
        , ("rts_evalStableIO", rtsEvalStableIOFunction opts)
        , ( "rts_evalStableIO_wrapper"
          , generateWrapperFunction "rts_evalStableIO" $
            rtsEvalStableIOFunction opts)
        , ("rts_getSchedStatus", rtsGetSchedStatusFunction opts)
        , ( "rts_getSchedStatus_wrapper"
          , generateWrapperFunction "rts_getSchedStatus" $
            rtsGetSchedStatusFunction opts)
        , ("rts_checkSchedStatus", rtsCheckSchedStatusFunction opts)
        , ( "rts_checkSchedStatus_wrapper"
          , generateWrapperFunction "rts_checkSchedStatus" $
            rtsCheckSchedStatusFunction opts)
        , ("setTSOLink", setTSOLinkFunction opts)
        , ("setTSOPrev", setTSOPrevFunction opts)
        , ("threadStackOverflow", threadStackOverflowFunction opts)
        , ("pushOnRunQueue", pushOnRunQueueFunction opts)
        , ("scheduleWaitThread", scheduleWaitThreadFunction opts)
        , ("createThread", createThreadFunction opts)
        , ("createGenThread", createGenThreadFunction opts)
        , ("createIOThread", createIOThreadFunction opts)
        , ("createStrictIOThread", createStrictIOThreadFunction opts)
        , ("malloc", mallocFunction opts)
        , ("memcpy", memcpyFunction opts)
        , ("allocate", allocateFunction opts)
        , ( "allocate_wrapper"
          , generateWrapperFunction "allocate" $ allocateFunction opts)
        , ("allocGroupOnNode", allocGroupOnNodeFunction opts)
        , ("getMBlocks", getMBlocksFunction opts)
        , ("free", freeFunction opts)
        , ("newCAF", newCAFFunction opts)
        , ("StgRun", stgRunFunction opts)
        , ("StgReturn", stgReturnFunction opts)
        , ("getStablePtr", getStablePtrWrapperFunction opts)
        , ( "getStablePtr_wrapper"
          , generateWrapperFunction "getStablePtr" $
            getStablePtrWrapperFunction opts)
        , ("deRefStablePtr", deRefStablePtrWrapperFunction opts)
        , ( "deRefStablePtr_wrapper"
          , generateWrapperFunction "deRefStablePtr" $
            deRefStablePtrWrapperFunction opts)
        , ("hs_free_stable_ptr", freeStablePtrWrapperFunction opts)
        , ( "hs_free_stable_ptr_wrapper"
          , generateWrapperFunction "hs_free_stable_ptr" $
            freeStablePtrWrapperFunction opts)
        , ("rts_mkBool", rtsMkBoolFunction opts)
        , ( "rts_mkBool_wrapper"
          , generateWrapperFunction "rts_mkBool" $ rtsMkBoolFunction opts)
        , ("rts_mkDouble", rtsMkDoubleFunction opts)
        , ( "rts_mkDouble_wrapper"
          , generateWrapperFunction "rts_mkDouble" $ rtsMkDoubleFunction opts)
        , ("rts_mkChar", rtsMkCharFunction opts)
        , ( "rts_mkChar_wrapper"
          , generateWrapperFunction "rts_mkChar" $ rtsMkCharFunction opts)
        , ("rts_mkInt", rtsMkIntFunction opts)
        , ( "rts_mkInt_wrapper"
          , generateWrapperFunction "rts_mkInt" $ rtsMkIntFunction opts)
        , ("rts_mkWord", rtsMkWordFunction opts)
        , ( "rts_mkWord_wrapper"
          , generateWrapperFunction "rts_mkWord" $ rtsMkWordFunction opts)
        , ("rts_mkPtr", rtsMkPtrFunction opts)
        , ( "rts_mkPtr_wrapper"
          , generateWrapperFunction "rts_mkPtr" $ rtsMkPtrFunction opts)
        , ("rts_mkStablePtr", rtsMkStablePtrFunction opts)
        , ( "rts_mkStablePtr_wrapper"
          , generateWrapperFunction "rts_mkStablePtr" $
            rtsMkStablePtrFunction opts)
        , ("rts_getBool", rtsGetBoolFunction opts)
        , ( "rts_getBool_wrapper"
          , generateWrapperFunction "rts_getBool" $ rtsGetBoolFunction opts)
        , ("rts_getDouble", rtsGetDoubleFunction opts)
        , ( "rts_getDouble_wrapper"
          , generateWrapperFunction "rts_getDouble" $ rtsGetDoubleFunction opts)
        , ("rts_getChar", rtsGetCharFunction opts)
        , ( "rts_getChar_wrapper"
          , generateWrapperFunction "rts_getChar" $ rtsGetCharFunction opts)
        , ("rts_getInt", rtsGetIntFunction opts)
        , ( "rts_getInt_wrapper"
          , generateWrapperFunction "rts_getInt" $ rtsGetIntFunction opts)
        , ("rts_getWord", rtsGetIntFunction opts)
        , ( "rts_getWord_wrapper"
          , generateWrapperFunction "rts_getWord" $ rtsGetIntFunction opts)
        , ("rts_getPtr", rtsGetIntFunction opts)
        , ( "rts_getPtr_wrapper"
          , generateWrapperFunction "rts_getPtr" $ rtsGetIntFunction opts)
        , ("rts_getStablePtr", rtsGetIntFunction opts)
        , ( "rts_getStablePtr_wrapper"
          , generateWrapperFunction "rts_getStablePtr" $ rtsGetIntFunction opts)
        , ("loadI64", loadI64Function opts)
        , ( "loadI64_wrapper"
          , generateWrapperFunction "loadI64" $ loadI64Function opts)
        , ("print_i64", printI64Function opts)
        , ("print_f32", printF32Function opts)
        , ("print_f64", printF64Function opts)
        , ("__asterius_Load_Sp", getF64GlobalRegFunction opts Sp)
        , ("__asterius_Load_SpLim", getF64GlobalRegFunction opts SpLim)
        , ("__asterius_Load_Hp", getF64GlobalRegFunction opts Hp)
        , ("__asterius_Load_HpLim", getF64GlobalRegFunction opts HpLim)
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

rtsFunctionImports :: Bool -> [FunctionImport]
rtsFunctionImports debug =
  [ FunctionImport
    { internalName = "__asterius_" <> op <> "_" <> showSBS ft
    , externalModuleName = "Math"
    , externalBaseName = op
    , functionType = FunctionType {paramTypes = [ft], returnTypes = [ft]}
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
  [ FunctionImport
    { internalName = "__asterius_" <> op <> "_" <> showSBS ft
    , externalModuleName = "Math"
    , externalBaseName = op
    , functionType = FunctionType {paramTypes = [ft, ft], returnTypes = [ft]}
    }
  | ft <- [F32, F64]
  , op <- ["pow"]
  ] <>
  [ FunctionImport
      { internalName = "__asterius_newStablePtr"
      , externalModuleName = "rts"
      , externalBaseName = "newStablePtr"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_deRefStablePtr"
      , externalModuleName = "rts"
      , externalBaseName = "deRefStablePtr"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_freeStablePtr"
      , externalModuleName = "rts"
      , externalBaseName = "freeStablePtr"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "printI64"
      , externalModuleName = "rts"
      , externalBaseName = "printI64"
      , functionType = FunctionType {paramTypes = [I32, I32], returnTypes = []}
      }
  , FunctionImport
      { internalName = "printI64_with_sym"
      , externalModuleName = "rts"
      , externalBaseName = "printI64_with_sym"
      , functionType = FunctionType {paramTypes = [I32, I32], returnTypes = []}
      }
  , FunctionImport
      { internalName = "printF32"
      , externalModuleName = "rts"
      , externalBaseName = "print"
      , functionType = FunctionType {paramTypes = [F32], returnTypes = []}
      }
  , FunctionImport
      { internalName = "printF64"
      , externalModuleName = "rts"
      , externalBaseName = "print"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_errorI32"
      , externalModuleName = "rts"
      , externalBaseName = "panic"
      , functionType = FunctionType {paramTypes = [I32], returnTypes = []}
      }
  ] <>
  (if debug
     then [ FunctionImport
              { internalName = "__asterius_traceCmm"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_traceCmm"
              , functionType =
                  FunctionType {paramTypes = [F64], returnTypes = []}
              }
          , FunctionImport
              { internalName = "__asterius_traceCmmBlock"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_traceCmmBlock"
              , functionType =
                  FunctionType {paramTypes = [F64, I32], returnTypes = []}
              }
          , FunctionImport
              { internalName = "__asterius_traceCmmSetLocal"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_traceCmmSetLocal"
              , functionType =
                  FunctionType
                    {paramTypes = [F64, I32, I32, I32], returnTypes = []}
              }
          , FunctionImport
              { internalName = "__asterius_current_memory"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_current_memory"
              , functionType =
                  FunctionType {paramTypes = [I32], returnTypes = [I32]}
              }
          , FunctionImport
              { internalName = "__asterius_grow_memory"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_grow_memory"
              , functionType =
                  FunctionType {paramTypes = [I32, I32], returnTypes = [I32]}
              }
          , FunctionImport
              { internalName = "__asterius_load_i64"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_load_i64"
              , functionType =
                  FunctionType
                    {paramTypes = [I32, I32, I32, I32, I32], returnTypes = []}
              }
          , FunctionImport
              { internalName = "__asterius_store_i64"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_store_i64"
              , functionType =
                  FunctionType
                    {paramTypes = [I32, I32, I32, I32, I32], returnTypes = []}
              }
          , FunctionImport
              { internalName = "__asterius_debug_log_is_enabled"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_debug_log_is_enabled"
              , functionType =
                  FunctionType {paramTypes = [], returnTypes = [I32]}
              }
          , FunctionImport
              { internalName = "__asterius_debug_log_set_enabled"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_debug_log_set_enabled"
              , functionType =
                  FunctionType {paramTypes = [I32], returnTypes = []}
              }
          ] <>
          concat
            [ [ FunctionImport
                  { internalName = "__asterius_load_" <> k
                  , externalModuleName = "rts"
                  , externalBaseName = "__asterius_load_" <> k
                  , functionType =
                      FunctionType
                        {paramTypes = [I32, I32, I32, t], returnTypes = []}
                  }
              , FunctionImport
                  { internalName = "__asterius_store_" <> k
                  , externalModuleName = "rts"
                  , externalBaseName = "__asterius_store_" <> k
                  , functionType =
                      FunctionType
                        {paramTypes = [I32, I32, I32, t], returnTypes = []}
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

rtsAsteriusFunctionExports :: Bool -> [FunctionExport]
rtsAsteriusFunctionExports debug =
  [ FunctionExport {internalName = f <> "_wrapper", externalName = f}
  | f <-
      [ "allocate"
      , "loadI64"
      , "rts_mkBool"
      , "rts_mkDouble"
      , "rts_mkChar"
      , "rts_mkInt"
      , "rts_mkWord"
      , "rts_mkPtr"
      , "rts_mkStablePtr"
      , "rts_getBool"
      , "rts_getDouble"
      , "rts_getChar"
      , "rts_getInt"
      , "rts_getWord"
      , "rts_getPtr"
      , "rts_getStablePtr"
      , "rts_apply"
      , "rts_eval"
      , "rts_evalIO"
      , "rts_evalLazyIO"
      , "rts_evalStableIO"
      , "rts_getSchedStatus"
      , "rts_checkSchedStatus"
      , "getStablePtr"
      , "deRefStablePtr"
      , "hs_free_stable_ptr"
      ]
  ] <>
  [ FunctionExport {internalName = f, externalName = f}
  | f <-
      (if debug
         then [ "__asterius_Load_Sp"
              , "__asterius_Load_SpLim"
              , "__asterius_Load_Hp"
              , "__asterius_Load_HpLim"
              ]
         else []) <>
      ["hs_init", "main"]
  ]

emitErrorMessage :: [ValueType] -> SBS.ShortByteString -> Expression
emitErrorMessage vts msg =
  EmitErrorMessage
    { errorMessage = ErrorMessage {unErrorMessage = msg}
    , phantomReturnTypes = vts
    }

assert :: BuiltinsOptions -> Expression -> EDSL ()
assert BuiltinsOptions {..} cond =
  when tracing $
  if' [] cond mempty $
  emit $
  emitErrorMessage [] $
  "Assertion failure, condition expression: " <> showSBS cond

generateWrapperFunction ::
     AsteriusEntitySymbol -> AsteriusFunction -> AsteriusFunction
generateWrapperFunction func_sym AsteriusFunction { functionType = FunctionType {..}
                                                  , ..
                                                  } =
  AsteriusFunction
    { functionType =
        FunctionType
          { paramTypes =
              [ wrapper_param_type
              | (_, wrapper_param_type, _) <- wrapper_param_types
              ]
          , returnTypes = wrapper_return_types
          }
    , body =
        to_wrapper_return_types $
        Call
          { target = func_sym
          , operands =
              [ from_wrapper_param_type
                GetLocal {index = i, valueType = wrapper_param_type}
              | (i, wrapper_param_type, from_wrapper_param_type) <-
                  wrapper_param_types
              ]
          , callReturnTypes = returnTypes
          }
    }
  where
    wrapper_param_types =
      [ case param_type of
        I64 -> (i, F64, Unary TruncUFloat64ToInt64)
        _ -> (i, param_type, id)
      | (i, param_type) <- zip [0 ..] paramTypes
      ]
    (wrapper_return_types, to_wrapper_return_types) =
      case returnTypes of
        [I64] -> ([F64], Unary ConvertUInt64ToFloat64)
        _ -> (returnTypes, id)

mainFunction, hsInitFunction, rtsApplyFunction, rtsEvalFunction, rtsEvalIOFunction, rtsEvalLazyIOFunction, rtsEvalStableIOFunction, rtsGetSchedStatusFunction, rtsCheckSchedStatusFunction, setTSOLinkFunction, setTSOPrevFunction, threadStackOverflowFunction, pushOnRunQueueFunction, scheduleWaitThreadFunction, createThreadFunction, createGenThreadFunction, createIOThreadFunction, createStrictIOThreadFunction, mallocFunction, memcpyFunction, allocateFunction, allocGroupOnNodeFunction, getMBlocksFunction, freeFunction, newCAFFunction, stgRunFunction, stgReturnFunction, getStablePtrWrapperFunction, deRefStablePtrWrapperFunction, freeStablePtrWrapperFunction, rtsMkBoolFunction, rtsMkDoubleFunction, rtsMkCharFunction, rtsMkIntFunction, rtsMkWordFunction, rtsMkPtrFunction, rtsMkStablePtrFunction, rtsGetBoolFunction, rtsGetDoubleFunction, rtsGetCharFunction, rtsGetIntFunction, loadI64Function, printI64Function, printF32Function, printF64Function ::
     BuiltinsOptions -> AsteriusFunction
mainFunction BuiltinsOptions {..} =
  runEDSL [] $
  call "rts_evalLazyIO" [mainCapability, symbol "Main_main_closure", constI64 0]

initCapability :: Expression -> Expression -> EDSL ()
initCapability cap i = do
  storeI32 cap offset_Capability_no i
  storeI32 cap offset_Capability_node $ constI32 0
  storeI8 cap offset_Capability_in_haskell $ constI32 0
  storeI32 cap offset_Capability_idle $ constI32 0
  storeI8 cap offset_Capability_disabled $ constI32 0
  storeI64 cap offset_Capability_run_queue_hd endTSOQueue
  storeI64 cap offset_Capability_run_queue_tl endTSOQueue
  storeI32 cap offset_Capability_n_run_queue $ constI32 0
  storeI64 cap offset_Capability_total_allocated $ constI64 0
  storeI64 cap (offset_Capability_f + offset_StgFunTable_stgEagerBlackholeInfo) $
    symbol "__stg_EAGER_BLACKHOLE_info"
  storeI64 cap (offset_Capability_f + offset_StgFunTable_stgGCEnter1) $
    symbol "__stg_gc_enter_1"
  storeI64 cap (offset_Capability_f + offset_StgFunTable_stgGCFun) $
    symbol "__stg_gc_fun"
  storeI64 cap offset_Capability_weak_ptr_list_hd $ constI64 0
  storeI64 cap offset_Capability_weak_ptr_list_tl $ constI64 0
  storeI64 cap offset_Capability_free_tvar_watch_queues $
    symbol "stg_END_STM_WATCH_QUEUE_closure"
  storeI64 cap offset_Capability_free_trec_chunks $
    symbol "stg_END_STM_CHUNK_LIST_closure"
  storeI64 cap offset_Capability_free_trec_headers $
    symbol "stg_NO_TREC_closure"
  storeI32 cap offset_Capability_transaction_tokens $ constI32 0
  storeI32 cap offset_Capability_context_switch $ constI32 0
  storeI64 cap offset_Capability_pinned_object_block $ constI64 0
  storeI64 cap offset_Capability_pinned_object_blocks $ constI64 0
  storeI64 cap (offset_Capability_r + offset_StgRegTable_rCCCS) $ constI64 0
  storeI64 cap (offset_Capability_r + offset_StgRegTable_rCurrentTSO) $
    constI64 0

hsInitFunction BuiltinsOptions {..} =
  runEDSL [] $ do
    initCapability mainCapability (constI32 0)
    bd <- call' "allocGroupOnNode" [constI32 0, constI64 nurseryGroups] I64
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

rtsEvalHelper :: BuiltinsOptions -> AsteriusEntitySymbol -> EDSL ()
rtsEvalHelper BuiltinsOptions {..} create_thread_func_sym = do
  [cap, p, ret] <- params [I64, I64, I64]
  tso <-
    call'
      create_thread_func_sym
      [cap, constI64 $ roundup_bytes_to_words threadStateSize, p]
      I64
  call "scheduleWaitThread" [tso, ret, cap]

rtsApplyFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [cap, f, arg] <- params [I64, I64, I64]
    ap <-
      call'
        "allocate"
        [cap, constI64 $ roundup_bytes_to_words sizeof_StgThunk + 2]
        I64
    storeI64 ap 0 $ symbol "stg_ap_2_upd_info"
    storeI64 ap offset_StgThunk_payload f
    storeI64 ap (offset_StgThunk_payload + 8) arg
    emit ap

rtsEvalFunction opts = runEDSL [] $ rtsEvalHelper opts "createGenThread"

rtsEvalIOFunction opts = runEDSL [] $ rtsEvalHelper opts "createStrictIOThread"

rtsEvalLazyIOFunction opts = runEDSL [] $ rtsEvalHelper opts "createIOThread"

rtsEvalStableIOFunction BuiltinsOptions {..} =
  runEDSL [] $ do
    [cap, s, ret] <- params [I64, I64, I64]
    p <- call' "deRefStablePtr" [s] I64
    tso <-
      call'
        "createStrictIOThread"
        [cap, constI64 $ roundup_bytes_to_words threadStateSize, p]
        I64
    storeI32 tso offset_StgTSO_flags $
      loadI32 tso offset_StgTSO_flags `orInt32` constI32 tso_BLOCKEX `orInt32`
      constI32 tso_INTERRUPTIBLE
    rp <- call' "allocate" [cap, constI64 1] I64
    call "scheduleWaitThread" [tso, rp, cap]
    stat <- call' "rts_getSchedStatus" [cap] I32
    if'
      []
      ((stat `eqInt32` constI32 scheduler_Success) `andInt32`
       (ret `neInt64` constI64 0))
      (call' "getStablePtr" [loadI64 rp 0] I64 >>= storeI64 ret 0)
      mempty

rtsGetSchedStatusFunction _ =
  runEDSL [I32] $ do
    setReturnTypes [I32]
    cap <- param I64
    emit $
      loadI32
        (loadI64 (loadI64 cap offset_Capability_running_task) offset_Task_incall)
        offset_InCall_rstat

rtsCheckSchedStatusFunction _ =
  runEDSL [] $ do
    cap <- param I64
    stat <- call' "rts_getSchedStatus" [cap] I32
    if' [] (stat `eqInt32` constI32 scheduler_Success) mempty $
      emit $
      emitErrorMessage
        []
        "rts_checkSchedStatus failed: illegal scheduler status code"

appendToRunQueue :: BuiltinsOptions -> Expression -> Expression -> EDSL ()
appendToRunQueue opts cap tso = do
  assert opts $ loadI64 tso offset_StgTSO__link `eqInt64` endTSOQueue
  if'
    []
    (loadI64 cap offset_Capability_run_queue_hd `eqInt64` endTSOQueue)
    (do storeI64 cap offset_Capability_run_queue_hd tso
        storeI64
          tso
          (offset_StgTSO_block_info + offset_StgTSOBlockInfo_prev)
          endTSOQueue)
    (do call "setTSOLink" [cap, loadI64 cap offset_Capability_run_queue_tl, tso]
        call "setTSOPrev" [cap, tso, loadI64 cap offset_Capability_run_queue_tl])
  storeI64 cap offset_Capability_run_queue_tl tso
  storeI32 cap offset_Capability_n_run_queue $
    loadI32 cap offset_Capability_n_run_queue `addInt32` constI32 1

setTSOLinkFunction _ =
  runEDSL [] $ do
    [_, tso, target] <- params [I64, I64, I64]
    if'
      []
      (eqZInt32 (loadI32 tso offset_StgTSO_dirty))
      (storeI32 tso offset_StgTSO_dirty (constI32 1))
      mempty
    storeI64 tso offset_StgTSO__link target

setTSOPrevFunction _ =
  runEDSL [] $ do
    [_, tso, target] <- params [I64, I64, I64]
    if'
      []
      (eqZInt32 (loadI32 tso offset_StgTSO_dirty))
      (storeI32 tso offset_StgTSO_dirty (constI32 1))
      mempty
    storeI64 tso (offset_StgTSO_block_info + offset_StgTSOBlockInfo_prev) target

isBoundTask :: Expression -> Expression
isBoundTask task =
  loadI64 (loadI64 task offset_Task_incall) offset_InCall_tso `neInt64`
  constI64 0

emptyRunQueue :: Expression -> Expression
emptyRunQueue cap = eqZInt32 $ loadI32 cap offset_Capability_n_run_queue

scheduleDoGC :: Expression -> Expression -> Expression -> EDSL ()
scheduleDoGC _ _ _ =
  emit $ emitErrorMessage [] "scheduleDoGC failed: unimplemented"

popRunQueue :: BuiltinsOptions -> Expression -> EDSL Expression
popRunQueue opts cap = do
  t <- i64Local $ loadI64 cap offset_Capability_run_queue_hd
  assert opts $ t `neInt64` endTSOQueue
  storeI64 cap offset_Capability_run_queue_hd $ loadI64 t offset_StgTSO__link
  if'
    []
    (loadI64 t offset_StgTSO__link `neInt64` endTSOQueue)
    (storeI64
       (loadI64 t offset_StgTSO__link)
       (offset_StgTSO_block_info + offset_StgTSOBlockInfo_prev)
       endTSOQueue)
    mempty
  storeI64 t offset_StgTSO__link endTSOQueue
  if'
    []
    (loadI64 cap offset_Capability_run_queue_hd `eqInt64` endTSOQueue)
    (storeI64 cap offset_Capability_run_queue_tl endTSOQueue)
    mempty
  storeI32 cap offset_Capability_n_run_queue $
    loadI32 cap offset_Capability_n_run_queue `subInt32` constI32 1
  pure t

deleteThread :: Expression -> EDSL ()
deleteThread _ = emit $ emitErrorMessage [] "deleteThread failed: unimplemented"

dirtyTSO :: Expression -> Expression -> EDSL ()
dirtyTSO _ tso =
  if'
    []
    (eqZInt32 $ loadI32 tso offset_StgTSO_dirty)
    (storeI32 tso offset_StgTSO_dirty $ constI32 1)
    mempty

dirtySTACK :: Expression -> Expression -> EDSL ()
dirtySTACK _ stack =
  if'
    []
    (eqZInt32 $ loadI32 stack offset_StgStack_dirty)
    (storeI32 stack offset_StgStack_dirty $ constI32 1)
    mempty

scheduleHandleHeapOverflow :: Expression -> Expression -> EDSL Expression
scheduleHandleHeapOverflow _ _ =
  pure $
  emitErrorMessage [I32] "scheduleHandleHeapOverflow failed: unimplemented"

threadStackOverflowFunction _ =
  runEDSL [] $ do
    _ <- params [I64, I64]
    emit $ emitErrorMessage [] "threadStackOverflow failed: unimplemented"

pushOnRunQueueFunction _ =
  runEDSL [] $ do
    [cap, tso] <- params [I64, I64]
    call "setTSOLink" [cap, tso, loadI64 cap offset_Capability_run_queue_hd]
    storeI64
      tso
      (offset_StgTSO_block_info + offset_StgTSOBlockInfo_prev)
      endTSOQueue
    if'
      []
      (loadI64 cap offset_Capability_run_queue_hd `neInt64` endTSOQueue)
      (call "setTSOPrev" [cap, loadI64 cap offset_Capability_run_queue_hd, tso])
      mempty
    storeI64 cap offset_Capability_run_queue_hd tso
    if'
      []
      (loadI64 cap offset_Capability_run_queue_tl `eqInt64` endTSOQueue)
      (storeI64 cap offset_Capability_run_queue_tl tso)
      mempty
    storeI32 cap offset_Capability_n_run_queue $
      loadI32 cap offset_Capability_n_run_queue `addInt32` constI32 1

scheduleHandleYield :: Expression -> Expression -> Expression -> EDSL Expression
scheduleHandleYield _ _ _ =
  pure $ emitErrorMessage [I32] "scheduleHandleYield failed: unimplemented"

scheduleHandleThreadBlocked :: Expression -> EDSL ()
scheduleHandleThreadBlocked _ =
  emit $ emitErrorMessage [] "scheduleHandleThreadBlock failed: unimplemented"

scheduleHandleThreadFinished ::
     BuiltinsOptions
  -> Expression
  -> Expression
  -> Expression
  -> EDSL Expression
scheduleHandleThreadFinished opts cap task t = do
  r <- i32MutLocal
  block' [] $ \ret_lbl ->
    if'
      []
      (loadI64 t offset_StgTSO_bound `neInt64` constI64 0)
      (do if'
            []
            (loadI64 t offset_StgTSO_bound `neInt64`
             loadI64 task offset_Task_incall)
            (do appendToRunQueue opts cap t
                putLVal r $ constI32 0
                break' ret_lbl Nothing)
            mempty
          assert opts $
            loadI64 (loadI64 task offset_Task_incall) offset_InCall_tso `eqInt64`
            t
          if'
            []
            (loadI16 t offset_StgTSO_what_next `eqInt32`
             constI32 next_ThreadComplete)
            (do if'
                  []
                  (loadI64 (loadI64 task offset_Task_incall) offset_InCall_ret `neInt64`
                   constI64 0)
                  (storeI64
                     (loadI64
                        (loadI64 task offset_Task_incall)
                        offset_InCall_ret)
                     0 $
                   loadI64
                     (loadI64
                        (loadI64
                           (loadI64
                              (loadI64 task offset_Task_incall)
                              offset_InCall_tso)
                           offset_StgTSO_stackobj)
                        offset_StgStack_sp)
                     8)
                  mempty
                storeI32 (loadI64 task offset_Task_incall) offset_InCall_rstat $
                  constI32 scheduler_Success)
            (do if'
                  []
                  (loadI64 (loadI64 task offset_Task_incall) offset_InCall_ret `neInt64`
                   constI64 0)
                  (storeI64
                     (loadI64
                        (loadI64 task offset_Task_incall)
                        offset_InCall_ret)
                     0 $
                   constI64 0)
                  mempty
                if'
                  []
                  (loadI64 (symbol "sched_state") 0 `geUInt64`
                   constI64 sched_SCHED_INTERRUPTING)
                  (if'
                     []
                     (loadI8 (symbol "heap_overflow") 0)
                     (storeI32
                        (loadI64 task offset_Task_incall)
                        offset_InCall_rstat $
                      constI32 scheduler_HeapExhausted)
                     (storeI32
                        (loadI64 task offset_Task_incall)
                        offset_InCall_rstat $
                      constI32 scheduler_Interrupted))
                  (storeI32
                     (loadI64 task offset_Task_incall)
                     offset_InCall_rstat $
                   constI32 scheduler_Killed))
          storeI64 t offset_StgTSO_bound $ constI64 0
          storeI64 (loadI64 task offset_Task_incall) offset_InCall_tso $
            constI64 0
          putLVal r $ constI32 1)
      (putLVal r $ constI32 0)
  pure $ getLVal r

scheduleNeedHeapProfile :: Expression -> EDSL Expression
scheduleNeedHeapProfile _ = pure $ constI32 0

schedule :: BuiltinsOptions -> Expression -> Expression -> EDSL ()
schedule opts cap task = do
  t <- i64MutLocal
  ret <- i32MutLocal
  ready_to_gc <- i32MutLocal
  block' [] $ \sched_block_lbl ->
    loop' [] $ \sched_loop_lbl -> do
      if'
        []
        (loadI8 cap offset_Capability_in_haskell)
        (emit
           (emitErrorMessage
              []
              "schedule failed: scheduler reentered from Haskell"))
        mempty
      switchI64 (loadI64 (symbol "sched_state") 0) $ \brake ->
        ( [ (sched_SCHED_RUNNING, brake)
          , ( sched_SCHED_INTERRUPTING
            , do scheduleDoGC cap task (constI32 1)
                 assert opts $
                   loadI64 (symbol "sched_state") 0 `eqInt64`
                   constI64 sched_SCHED_SHUTTING_DOWN)
          , ( sched_SCHED_SHUTTING_DOWN
            , if'
                []
                (eqZInt32 (isBoundTask task) `andInt32` emptyRunQueue cap)
                (break' sched_block_lbl Nothing)
                brake)
          ]
        , emit $ emitErrorMessage [] "schedule failed: illegal sched_state")
      if'
        []
        (emptyRunQueue cap)
        (assert opts $
         loadI64 (symbol "sched_state") 0 `geUInt64`
         constI64 sched_SCHED_INTERRUPTING)
        mempty
      popRunQueue opts cap >>= putLVal t
      if'
        []
        ((loadI64 (symbol "sched_state") 0 `geUInt64`
          constI64 sched_SCHED_INTERRUPTING) `andInt32`
         notInt32
           ((loadI16 (getLVal t) offset_StgTSO_what_next `eqInt32`
             constI32 next_ThreadComplete) `orInt32`
            (loadI16 (getLVal t) offset_StgTSO_what_next `eqInt32`
             constI32 next_ThreadKilled)))
        (deleteThread (getLVal t))
        mempty
      loop' [] $ \run_thread_lbl -> do
        storeI64
          cap
          (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
          (getLVal t)
        assert opts $ loadI64 (getLVal t) offset_StgTSO_cap `eqInt64` cap
        assert opts $
          (loadI64
             (loadI64
                (loadI64 (getLVal t) offset_StgTSO_bound)
                offset_InCall_task)
             offset_Task_cap `eqInt64`
           cap) `orInt32`
          eqZInt64 (loadI64 (getLVal t) offset_StgTSO_bound)
        prev_what_next <- i32Local $ loadI16 (getLVal t) offset_StgTSO_what_next
        storeI32 cap offset_Capability_interrupt $ constI32 0
        storeI8 cap offset_Capability_in_haskell $ constI32 1
        storeI32 cap offset_Capability_idle $ constI32 0
        dirtyTSO cap (getLVal t)
        dirtySTACK cap (loadI64 (getLVal t) offset_StgTSO_stackobj)
        switchI64 (loadI64 (symbol "recent_activity") 0) $ \brake ->
          ( [ ( recent_ACTIVITY_DONE_GC
              , do storeI64
                     (symbol "recent_activity")
                     0
                     (constI64 recent_ACTIVITY_YES)
                   brake)
            , (recent_ACTIVITY_INACTIVE, brake)
            ]
          , storeI64 (symbol "recent_activity") 0 (constI64 recent_ACTIVITY_YES))
        switchI64 (extendUInt32 prev_what_next) $ \brake ->
          ( [ (next_ThreadKilled, mempty)
            , ( next_ThreadComplete
              , do putLVal ret $ constI32 ret_ThreadFinished
                   brake)
            , ( next_ThreadRunGHC
              , do r <-
                     call'
                       "StgRun"
                       [ symbol "stg_returnToStackTop"
                       , mainCapability `addInt64` constI64 offset_Capability_r
                       ]
                       I64
                   putLVal ret $ wrapInt64 $ loadI64 r offset_StgRegTable_rRet
                   brake)
            ]
          , emit $ emitErrorMessage [] "schedule failed: errIllegalPrevWhatNext")
        storeI8 cap offset_Capability_in_haskell $ constI32 0
        putLVal t $
          loadI64 cap (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
        storeI64 cap (offset_Capability_r + offset_StgRegTable_rCurrentTSO) $
          constI64 0
        assert opts $ loadI64 (getLVal t) offset_StgTSO_cap `eqInt64` cap
        putLVal ready_to_gc $ constI32 0
        switchI64 (extendUInt32 (getLVal ret)) $ \brake ->
          ( [ ( ret_HeapOverflow
              , do scheduleHandleHeapOverflow cap (getLVal t) >>=
                     putLVal ready_to_gc
                   brake)
            , ( ret_StackOverflow
              , do call "threadStackOverflow" [cap, getLVal t]
                   call "pushOnRunQueue" [cap, getLVal t]
                   brake)
            , ( ret_ThreadYielding
              , do scheduleHandleYield cap (getLVal t) prev_what_next >>=
                     break' run_thread_lbl . Just
                   brake)
            , ( ret_ThreadBlocked
              , do scheduleHandleThreadBlocked (getLVal t)
                   brake)
            , ( ret_ThreadFinished
              , do scheduleHandleThreadFinished opts cap task (getLVal t) >>=
                     break' sched_block_lbl . Just
                   brake)
            ]
          , emit $
            emitErrorMessage [] "schedule failed: errIllegalThreadReturnCode")
        need_heap_profile <- scheduleNeedHeapProfile (getLVal ready_to_gc)
        if'
          []
          (getLVal ready_to_gc `orInt32` need_heap_profile)
          (scheduleDoGC cap task (constI32 0))
          mempty
        break' sched_loop_lbl Nothing

scheduleWaitThreadFunction opts =
  runEDSL [] $ do
    [tso, ret, cap] <- params [I64, I64, I64]
    task <- i64Local $ loadI64 cap offset_Capability_running_task
    storeI64 tso offset_StgTSO_bound $ loadI64 task offset_Task_incall
    storeI64 tso offset_StgTSO_cap cap
    incall <- i64Local $ loadI64 task offset_Task_incall
    storeI64 incall offset_InCall_tso tso
    storeI64 incall offset_InCall_ret ret
    storeI32 incall offset_InCall_rstat $ constI32 scheduler_NoStatus
    appendToRunQueue opts cap tso
    schedule opts cap task
    assert opts $
      loadI32 (loadI64 task offset_Task_incall) offset_InCall_rstat `neInt32`
      constI32 scheduler_NoStatus

createThreadFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
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
    storeI64 stack_p offset_StgStack_sp $
      (stack_p `addInt64` constI64 offset_StgStack_stack) `addInt64`
      stack_size_w
    storeI32 stack_p offset_StgStack_dirty $ constI32 1
    storeI64 tso_p 0 $ symbol "stg_TSO_info"
    storeI16 tso_p offset_StgTSO_what_next $ constI32 next_ThreadRunGHC
    storeI16 tso_p offset_StgTSO_why_blocked $ constI32 blocked_NotBlocked
    storeI64
      tso_p
      (offset_StgTSO_block_info + offset_StgTSOBlockInfo_closure)
      endTSOQueue
    storeI64 tso_p offset_StgTSO_blocked_exceptions endTSOQueue
    storeI64 tso_p offset_StgTSO_bq endTSOQueue
    storeI32 tso_p offset_StgTSO_flags $ constI32 0
    storeI32 tso_p offset_StgTSO_dirty $ constI32 1
    storeI64 tso_p offset_StgTSO__link endTSOQueue
    storeI32 tso_p offset_StgTSO_saved_errno $ constI32 0
    storeI64 tso_p offset_StgTSO_bound $ constI64 0
    storeI64 tso_p offset_StgTSO_cap cap
    storeI64 tso_p offset_StgTSO_stackobj stack_p
    storeI32 tso_p offset_StgTSO_tot_stack_size $ wrapInt64 stack_size_w
    storeI64 tso_p offset_StgTSO_alloc_limit (constI64 0)
    storeI64 tso_p offset_StgTSO_trec $ symbol "stg_NO_TREC_closure"
    storeI64 stack_p offset_StgStack_sp $
      loadI64 stack_p offset_StgStack_sp `subInt64`
      constI64 (8 * roundup_bytes_to_words sizeof_StgStopFrame)
    storeI64 (loadI64 stack_p offset_StgStack_sp) 0 $
      symbol "stg_stop_thread_info"
    emit tso_p

pushClosure :: Expression -> Expression -> EDSL ()
pushClosure tso c = do
  stack_p <- i64Local $ loadI64 tso offset_StgTSO_stackobj
  storeI64 stack_p offset_StgStack_sp $
    loadI64 stack_p offset_StgStack_sp `subInt64` constI64 8
  storeI64 (loadI64 stack_p offset_StgStack_sp) 0 c

createThreadHelper :: (Expression -> [Expression]) -> EDSL ()
createThreadHelper mk_closures = do
  setReturnTypes [I64]
  [cap, stack_size, closure] <- params [I64, I64, I64]
  t <- call' "createThread" [cap, stack_size] I64
  for_ (mk_closures closure) $ pushClosure t
  emit t

createGenThreadFunction _ =
  runEDSL [I64] $
  createThreadHelper $ \closure -> [closure, symbol "stg_enter_info"]

createIOThreadFunction _ =
  runEDSL [I64] $
  createThreadHelper $ \closure ->
    [symbol "stg_ap_v_info", closure, symbol "stg_enter_info"]

createStrictIOThreadFunction _ =
  runEDSL [I64] $
  createThreadHelper $ \closure ->
    [ symbol "stg_forceIO_info"
    , symbol "stg_ap_v_info"
    , closure
    , symbol "stg_enter_info"
    ]

mallocFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    size <- param I64
    call' "allocate" [mainCapability, roundupBytesToWords size] I64 >>= emit

memcpyFunction _ =
  runEDSL [I64] $ do
    [dest, src, count] <- params [I64, I64, I64]
    i <- i64MutLocal
    putLVal i $ constI64 0
    whileLoop [] (getLVal i `ltUInt64` count) $ do
      storeI8 (dest `addInt64` getLVal i) 0 $
        loadI8 (src `addInt64` getLVal i) 0
      putLVal i $ getLVal i `addInt64` constI64 1

allocateFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [_, n] <- params [I64, I64]
    new_hp <- i64Local $ getLVal hp `addInt64` (n `mulInt64` constI64 8)
    if'
      []
      (new_hp `gtUInt64` getLVal hpLim)
      (emit $ emitErrorMessage [] "allocate failed: errHeapOverflow")
      mempty
    old_hp <- i64Local $ getLVal hp
    putLVal hp new_hp
    storeI64
      (loadI64 (getLVal baseReg) offset_StgRegTable_rCurrentAlloc)
      offset_bdescr_free
      new_hp
    emit old_hp

blocksToMBlocks :: Expression -> EDSL Expression
blocksToMBlocks n = do
  r <- i64MutLocal
  if'
    []
    (n `leUInt64` constI64 blocks_per_mblock)
    (putLVal r (constI64 1))
    (putLVal
       r
       (constI64 1 `addInt64`
        ((n `mulInt64` constI64 block_size) `divUInt64` constI64 mblock_size)))
  pure $ getLVal r

initGroup :: Expression -> EDSL ()
initGroup hd = do
  storeI64 hd offset_bdescr_free $ loadI64 hd offset_bdescr_start
  storeI64 hd offset_bdescr_link $ constI64 0

allocGroupOnNodeFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [node, n] <- params [I32, I64]
    mblocks <- blocksToMBlocks n
    bd <- allocMegaGroup node mblocks
    initGroup bd
    emit bd

getMBlocksFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    n <- param I32
    ret <-
      i64Local $
      extendUInt32 $
      growMemory (n `mulInt32` constI32 (mblock_size `div` wasmPageSize)) `mulInt32`
      constI32 wasmPageSize
    emit $ ConstI64 (dataTag `shiftL` 32) `orInt64` ret

mblockGroupBlocks :: Expression -> Expression
mblockGroupBlocks n =
  constI64 blocks_per_mblock `addInt64`
  ((n `subInt64` constI64 1) `mulInt64` constI64 (mblock_size `div` block_size))

initMBlock :: Expression -> Expression -> EDSL ()
initMBlock mblock node = do
  block <- i64MutLocal
  putLVal block $ mblock `addInt64` constI64 offset_first_block
  bd <- i64MutLocal
  putLVal bd $ mblock `addInt64` constI64 offset_first_bdescr
  last_block <- i64Local $ mblock `addInt64` constI64 offset_last_block
  whileLoop [] (getLVal block `leUInt64` last_block) $ do
    storeI64 (getLVal bd) offset_bdescr_start (getLVal block)
    storeI16 (getLVal bd) offset_bdescr_node node
    putLVal bd $ getLVal bd `addInt64` constI64 sizeof_bdescr
    putLVal block $ getLVal block `addInt64` constI64 block_size

allocMegaGroup :: Expression -> Expression -> EDSL Expression
allocMegaGroup node mblocks = do
  mblock <- call' "getMBlocks" [wrapInt64 mblocks] I64
  initMBlock mblock node
  bd <- i64Local $ mblock `addInt64` constI64 offset_first_bdescr
  storeI32 bd offset_bdescr_blocks $ wrapI64 $ mblockGroupBlocks mblocks
  pure bd

freeFunction _ =
  runEDSL [] $ do
    _ <- param I64
    emit $ emitErrorMessage [] "free failed: unimplemented"

newCAFFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
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
  runEDSL [I64] $ do
    setReturnTypes [I64]
    f <- mutParam I64
    _ <- param I64
    loop' [] $ \loop_lbl ->
      if' [] (eqZInt64 (getLVal f)) mempty $ do
        f' <-
          callIndirect'
            (getLVal f `subInt64` constI64 1)
            []
            (FunctionType [] [I64])
        putLVal f f'
        break' loop_lbl Nothing
    emit $ getLVal r1

stgReturnFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    emit $ constI64 0

getStablePtrWrapperFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    obj64 <- param I64
    sp_f64 <-
      callImport'
        "__asterius_newStablePtr"
        [Unary ConvertUInt64ToFloat64 obj64]
        F64
    emit $ Unary TruncUFloat64ToInt64 sp_f64

deRefStablePtrWrapperFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    sp64 <- param I64
    obj_f64 <-
      callImport'
        "__asterius_deRefStablePtr"
        [Unary ConvertUInt64ToFloat64 sp64]
        F64
    emit $ Unary TruncUFloat64ToInt64 obj_f64

freeStablePtrWrapperFunction _ =
  runEDSL [] $ do
    sp64 <- param I64
    callImport "__asterius_freeStablePtr" [Unary ConvertUInt64ToFloat64 sp64]

rtsMkHelper :: BuiltinsOptions -> AsteriusEntitySymbol -> AsteriusFunction
rtsMkHelper _ con_sym =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [cap, i] <- params [I64, I64]
    p <- call' "allocate" [cap, constI64 2] I64
    storeI64 p 0 $ symbol con_sym
    storeI64 p 8 i
    emit p

rtsMkBoolFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [_, i] <- params [I64, I64]
    if'
      [I64]
      (eqZInt64 i)
      (emit $ symbol "ghczmprim_GHCziTypes_False_closure")
      (emit $ symbol "ghczmprim_GHCziTypes_True_closure")

rtsMkDoubleFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [cap, i] <- params [I64, F64]
    p <- call' "allocate" [cap, constI64 2] I64
    storeI64 p 0 $ symbol "ghczmprim_GHCziTypes_Dzh_con_info"
    storeF64 p 8 i
    emit p

rtsMkCharFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [cap, i] <- params [I64, I64]
    p <- call' "allocate" [cap, constI64 2] I64
    storeI64 p 0 $ symbol "ghczmprim_GHCziTypes_Czh_con_info"
    storeI64 p 8 i
    emit p

rtsMkIntFunction opts = rtsMkHelper opts "ghczmprim_GHCziTypes_Izh_con_info"

rtsMkWordFunction opts = rtsMkHelper opts "ghczmprim_GHCziTypes_Wzh_con_info"

rtsMkPtrFunction opts = rtsMkHelper opts "base_GHCziPtr_Ptr_con_info"

rtsMkStablePtrFunction opts =
  rtsMkHelper opts "base_GHCziStable_StablePtr_con_info"

unTagClosure :: Expression -> Expression
unTagClosure p = p `andInt64` constI64 0xFFFFFFFFFFFFFFF8

rtsGetBoolFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    p <- param I64
    emit $
      extendUInt32 $
      neInt32
        (constI32 0)
        (loadI32 (loadI64 (unTagClosure p) 0) offset_StgInfoTable_srt)

rtsGetDoubleFunction _ =
  runEDSL [F64] $ do
    setReturnTypes [F64]
    p <- param I64
    emit $ loadF64 (unTagClosure p) offset_StgClosure_payload

rtsGetCharFunction = rtsGetIntFunction

rtsGetIntFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    p <- param I64
    emit $ loadI64 (unTagClosure p) offset_StgClosure_payload

loadI64Function _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    p <- param I64
    emit $ loadI64 p 0

printI64Function _ =
  runEDSL [] $ do
    x <- param I64
    callImport "printI64" (cutI64 x)

printF32Function _ =
  runEDSL [] $ do
    x <- param F32
    callImport "printF32" [x]

printF64Function _ =
  runEDSL [] $ do
    x <- param F64
    callImport "printF64" [x]

getF64GlobalRegFunction ::
     BuiltinsOptions -> UnresolvedGlobalReg -> AsteriusFunction
getF64GlobalRegFunction _ gr =
  runEDSL [F64] $ do
    setReturnTypes [F64]
    emit $ Unary ConvertUInt64ToFloat64 $ getLVal $ global gr

loadWrapperFunction, storeWrapperFunction ::
     BuiltinsOptions -> BinaryenIndex -> ValueType -> AsteriusFunction
loadWrapperFunction _ b vt =
  AsteriusFunction
    { functionType = FunctionType {paramTypes = [I64, I32], returnTypes = [vt]}
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
                      cutI64 p <> [o] <>
                      (case vt of
                         I64 -> cutI64 v
                         _ -> [v])
                  , callImportReturnTypes = []
                  }
              , v
              ]
          , blockReturnTypes = [vt]
          }
    }
  where
    p = getLocalWord 0
    o = GetLocal {index = 1, valueType = I32}
    v =
      Load
        { signed = False
        , bytes = b
        , offset = 0
        , valueType = vt
        , ptr = wrapI64 p `addInt32` o
        }

storeWrapperFunction _ b vt =
  AsteriusFunction
    { functionType =
        FunctionType {paramTypes = [I64, I32, vt], returnTypes = []}
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
                      cutI64 p <> [o] <>
                      (case vt of
                         I64 -> cutI64 v
                         _ -> [v])
                  , callImportReturnTypes = []
                  }
              , Store
                  { bytes = b
                  , offset = 0
                  , ptr = wrapI64 p `addInt32` o
                  , value = v
                  , valueType = vt
                  }
              ]
          , blockReturnTypes = []
          }
    }
  where
    p = getLocalWord 0
    o = GetLocal {index = 1, valueType = I32}
    v = GetLocal {index = 2, valueType = vt}

wrapI64 :: Expression -> Expression
wrapI64 w = Unary {unaryOp = WrapInt64, operand0 = w}

getLocalWord :: BinaryenIndex -> Expression
getLocalWord i = GetLocal {index = i, valueType = I64}

offset_StgTSO_StgStack :: Int
offset_StgTSO_StgStack = 8 * roundup_bytes_to_words sizeof_StgTSO

cutI64 :: Expression -> [Expression]
cutI64 x =
  [ wrapI64 x
  , wrapI64 $
    Binary {binaryOp = ShrUInt64, operand0 = x, operand1 = ConstI64 32}
  ]
