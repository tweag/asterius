{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Builtins
  ( BuiltinsOptions (..),
    defaultBuiltinsOptions,
    rtsAsteriusModule,
    rtsFunctionImports,
    rtsFunctionExports,
    emitErrorMessage,
    generateWrapperFunction,
  )
where

import Asterius.Builtins.Barf
import Asterius.Builtins.Blackhole
import Asterius.Builtins.CMath
import Asterius.Builtins.Endianness
import Asterius.Builtins.Env
import Asterius.Builtins.Exports
import Asterius.Builtins.Posix
import Asterius.Builtins.Primitive
import Asterius.Builtins.Scheduler
import Asterius.Builtins.SM
import Asterius.Builtins.Sparks
import Asterius.Builtins.StgPrimFloat
import Asterius.Builtins.Time
import Asterius.EDSL
import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import qualified Data.ByteString as BS
import Data.Foldable
import Data.String
import Data.Word
import Language.Haskell.GHC.Toolkit.Constants

data BuiltinsOptions
  = BuiltinsOptions
      { progName :: String,
        debug, hasMain :: Bool
      }

defaultBuiltinsOptions :: BuiltinsOptions
defaultBuiltinsOptions = BuiltinsOptions
  { progName =
      error
        "Asterius.Builtins.defaultBuiltinsOptions: unknown progName",
    debug = False,
    hasMain = True
  }

rtsAsteriusModule :: BuiltinsOptions -> AsteriusModule
rtsAsteriusModule opts =
  mempty
    { staticsMap =
        SM.fromList
          [ ( "MainCapability",
              AsteriusStatics
                { asteriusStatics =
                    [ Serialized $ BS.pack $
                        replicate
                          (4 * roundup_bytes_to_words sizeof_Capability)
                          0
                    ]
                }
            ),
            ( "rts_stop_on_exception",
              AsteriusStatics
                { asteriusStatics = [Serialized $ encodeStorable (0 :: Word32)]
                }
            ),
            ( "n_capabilities",
              AsteriusStatics
                { asteriusStatics = [Serialized $ encodeStorable (1 :: Word32)]
                }
            ),
            ( "enabled_capabilities",
              AsteriusStatics
                { asteriusStatics = [Serialized $ encodeStorable (1 :: Word32)]
                }
            ),
            ( "prog_name",
              AsteriusStatics
                { asteriusStatics =
                    [ Serialized $
                        fromString (progName opts <> "\0")
                    ]
                }
            ),
            ( "__asterius_localeEncoding",
              AsteriusStatics
                { asteriusStatics = [Serialized "UTF-8\0"]
                }
            ),
            ( "__asterius_i64_slot",
              AsteriusStatics
                { asteriusStatics = [Serialized $ BS.pack $ replicate 8 0]
                }
            ),
            ("__asterius_pc", AsteriusStatics {
              asteriusStatics = [Serialized $ encodeStorable (0 :: Word32)]
            })
          ],
      functionMap =
        SM.fromList $
          map
            (\(func_sym, (_, func)) -> (func_sym, func))
            ( floatCBits
                <> unicodeCBits
            )
    }
    <> hsInitFunction opts
    <> createThreadFunction opts
    <> getThreadIdFunction opts
    <> genAllocateFunction opts "allocate"
    <> genAllocateFunction opts "allocateMightFail"
    <> allocatePinnedFunction opts
    <> newCAFFunction opts
    <> stgReturnFunction opts
    <> strlenFunction opts
    <> memchrFunction opts
    <> threadPausedFunction opts
    <> dirtyMutVarFunction opts
    <> dirtyMVarFunction opts
    <> dirtyStackFunction opts
    <> recordClosureMutatedFunction opts
    <> tryWakeupThreadFunction opts
    <> raiseExceptionHelperFunction opts
    <> suspendThreadFunction opts
    <> scheduleThreadFunction opts
    <> scheduleThreadOnFunction opts
    <> resumeThreadFunction opts
    <> performMajorGCFunction opts
    <> performGCFunction opts
    <> localeEncodingFunction opts
    <> isattyFunction opts
    <> fdReadyFunction opts
    <> rtsSupportsBoundThreadsFunction opts
    <> readFunction opts
    <> writeFunction opts
    -- Add in the module that contain functions which need to be
    -- exposed to the outside world. So add in the module, and
    -- the module wrapped by using `generateWrapperModule`.
    <> generateRtsExternalInterfaceModule opts
    <> generateWrapperModule (generateRtsExternalInterfaceModule opts)
    <> blackholeCBits
    <> generateWrapperModule blackholeCBits
    <> smCBits
    <> generateWrapperModule smCBits
    <> sparksCBits
    <> cmathCBits
    <> envCBits
    <> posixCBits
    <> stgPrimFloatCBits
    <> primitiveCBits
    <> endiannessCBits

-- Generate the module consisting of functions which need to be wrapped
-- for communication with the external runtime.
generateRtsExternalInterfaceModule :: BuiltinsOptions -> AsteriusModule
generateRtsExternalInterfaceModule opts =
  mempty
    <> rtsApplyFunction opts
    <> createIOThreadFunction opts
    <> createStrictIOThreadFunction opts
    <> scheduleTSOFunction opts
    <> rtsGetSchedStatusFunction opts
    <> rtsCheckSchedStatusFunction opts
    <> getStablePtrWrapperFunction opts
    <> deRefStablePtrWrapperFunction opts
    <> freeStablePtrWrapperFunction opts
    <> makeStableNameWrapperFunction opts
    <> rtsMkBoolFunction opts
    <> rtsMkDoubleFunction opts
    <> rtsMkCharFunction opts
    <> rtsMkIntFunction opts
    <> rtsMkWordFunction opts
    <> rtsMkPtrFunction opts
    <> rtsMkStablePtrFunction opts
    <> rtsMkJSValFunction opts
    <> rtsGetBoolFunction opts
    <> rtsGetDoubleFunction opts
    <> generateRtsGetIntFunction opts "rts_getChar"
    <> generateRtsGetIntFunction opts "rts_getInt"
    <> generateRtsGetIntFunction opts "rts_getWord"
    <> generateRtsGetIntFunction opts "rts_getPtr"
    <> generateRtsGetIntFunction opts "rts_getStablePtr"
    <> generateRtsGetIntFunction opts "rts_getJSVal"
    <> loadI64Function opts

rtsFunctionImports :: Bool -> [FunctionImport]
rtsFunctionImports debug =
  [ FunctionImport
           { internalName = "__asterius_newStablePtr",
             externalModuleName = "StablePtr",
             externalBaseName = "newStablePtr",
             functionType = FunctionType
               { paramTypes = [I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_deRefStablePtr",
             externalModuleName = "StablePtr",
             externalBaseName = "deRefStablePtr",
             functionType = FunctionType
               { paramTypes = [I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_freeStablePtr",
             externalModuleName = "StablePtr",
             externalBaseName = "freeStablePtr",
             functionType = FunctionType {paramTypes = [I32], returnTypes = []}
           },
         FunctionImport
           { internalName = "__asterius_makeStableName",
             externalModuleName = "StableName",
             externalBaseName = "makeStableName",
             functionType = FunctionType
               { paramTypes = [I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "print_i64",
             externalModuleName = "rts",
             externalBaseName = "printI64",
             functionType = FunctionType {paramTypes = [I64], returnTypes = []}
           },
         FunctionImport
           { internalName = "assert_eq_i64",
             externalModuleName = "rts",
             externalBaseName = "assertEqI64",
             functionType = FunctionType
               { paramTypes = [I64, I64],
                 returnTypes = []
               }
           },
         FunctionImport
           { internalName = "print_f32",
             externalModuleName = "rts",
             externalBaseName = "print",
             functionType = FunctionType {paramTypes = [F32], returnTypes = []}
           },
         FunctionImport
           { internalName = "print_f64",
             externalModuleName = "rts",
             externalBaseName = "print",
             functionType = FunctionType {paramTypes = [F64], returnTypes = []}
           },
         FunctionImport
           { internalName = "__asterius_newTSO",
             externalModuleName = "Scheduler",
             externalBaseName = "newTSO",
             functionType = FunctionType {paramTypes = [], returnTypes = [I32]}
           },
         FunctionImport
           { internalName = "__asterius_getTSOrstat",
             externalModuleName = "Scheduler",
             externalBaseName = "getTSOrstat",
             functionType = FunctionType
               { paramTypes = [I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_hpAlloc",
             externalModuleName = "HeapAlloc",
             externalBaseName = "hpAlloc",
             functionType = FunctionType
               { paramTypes = [I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_allocate",
             externalModuleName = "HeapAlloc",
             externalBaseName = "allocate",
             functionType = FunctionType
               { paramTypes = [I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_allocatePinned",
             externalModuleName = "HeapAlloc",
             externalBaseName = "allocatePinned",
             functionType = FunctionType
               { paramTypes = [I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_strlen",
             externalModuleName = "Memory",
             externalBaseName = "strlen",
             functionType = FunctionType
               { paramTypes = [I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "debugBelch2",
             externalModuleName = "Messages",
             externalBaseName = "debugBelch2",
             functionType = FunctionType
               { paramTypes = [I32, I32],
                 returnTypes = []
               }
           },
         FunctionImport
           { internalName = "__asterius_memchr",
             externalModuleName = "Memory",
             externalBaseName = "memchr",
             functionType = FunctionType
               { paramTypes = [I32, I32, I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_performGC",
             externalModuleName = "GC",
             externalBaseName = "performGC",
             functionType = FunctionType {paramTypes = [], returnTypes = []}
           },
         FunctionImport
           { internalName = "__asterius_raiseExceptionHelper",
             externalModuleName = "ExceptionHelper",
             externalBaseName = "raiseExceptionHelper",
             functionType = FunctionType
               { paramTypes = [I32, I32, I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_enqueueTSO",
             externalModuleName = "Scheduler",
             externalBaseName = "enqueueTSO",
             functionType = FunctionType {paramTypes = [I32], returnTypes = []}
           },
         FunctionImport
           { internalName = "__asterius_mul2",
             externalModuleName = "Integer",
             externalBaseName = "mul2",
             functionType = FunctionType
               { paramTypes = [I32, I32, I32, I32, I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_quotrem2_quotient",
             externalModuleName = "Integer",
             externalBaseName = "quotrem2_quotient",
             functionType = FunctionType
               { paramTypes = [I32, I32, I32, I32, I32, I32, I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_quotrem2_remainder",
             externalModuleName = "Integer",
             externalBaseName = "quotrem2_remainder",
             functionType = FunctionType
               { paramTypes = [I32, I32, I32, I32, I32, I32, I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_read",
             externalModuleName = "fs",
             externalBaseName = "read",
             functionType = FunctionType
               { paramTypes = [I32, I32, I32],
                 returnTypes = [I32]
               }
           },
         FunctionImport
           { internalName = "__asterius_write",
             externalModuleName = "fs",
             externalBaseName = "write",
             functionType = FunctionType
               { paramTypes = [I32, I32, I32],
                 returnTypes = [I32]
               }
           }
       ]
    <> ( if debug
           then
             concat
                 [ [ FunctionImport
                       { internalName = "__asterius_load_" <> k,
                         externalModuleName = "MemoryTrap",
                         externalBaseName = "load" <> k,
                         functionType = FunctionType
                           { paramTypes = [I32, I32, I32],
                             returnTypes = [t]
                           }
                       },
                     FunctionImport
                       { internalName = "__asterius_store_" <> k,
                         externalModuleName = "MemoryTrap",
                         externalBaseName = "store" <> k,
                         functionType = FunctionType
                           { paramTypes = [I32, I32, I32, t],
                             returnTypes = []
                           }
                       }
                   ]
                   | (k, t) <-
                       [ ("I8", I32),
                         ("I16", I32),
                         ("I32", I32),
                         ("I64", I64),
                         ("F32", F32),
                         ("F64", F64)
                       ]
                 ]
               <> [ FunctionImport
                      { internalName = "__asterius_load_" <> k1 <> "_" <> s <> b,
                        externalModuleName = "MemoryTrap",
                        externalBaseName = "load" <> k1 <> s <> b,
                        functionType = FunctionType
                          { paramTypes = [I32, I32, I32],
                            returnTypes = [t1]
                          }
                      }
                    | (k1, t1) <- [("I32", I32), ("I64", I64)],
                      s <- ["S", "U"],
                      b <- ["8", "16"]
                  ]
           else []
       )
    <> map
      (fst . snd)
      ( floatCBits <> unicodeCBits
      )
    <> schedulerImports
    <> exportsImports
    <> envImports
    <> posixImports
    <> timeImports
    <> primitiveImports
    <> barfImports

rtsFunctionExports :: Bool -> [FunctionExport]
rtsFunctionExports debug =
  [ FunctionExport {internalName = f <> "_wrapper", externalName = f}
    | f <-
        [ "loadI64",
          "rts_mkBool",
          "rts_mkDouble",
          "rts_mkChar",
          "rts_mkInt",
          "rts_mkWord",
          "rts_mkPtr",
          "rts_mkStablePtr",
          "rts_mkJSVal",
          "rts_getBool",
          "rts_getDouble",
          "rts_getChar",
          "rts_getInt",
          "rts_getWord",
          "rts_getPtr",
          "rts_getStablePtr",
          "rts_getJSVal",
          "rts_apply",
          "createStrictIOThread",
          "createIOThread",
          "scheduleTSO",
          "rts_getSchedStatus",
          "rts_checkSchedStatus",
          "getStablePtr",
          "deRefStablePtr",
          "hs_free_stable_ptr",
          "makeStableName",
          "growStack",
          "updateThunk"
        ]
  ]
    <> [ FunctionExport {internalName = "__asterius_" <> f, externalName = f}
         | f <- ["getTSOret", "getTSOrstat"]
       ]
    <> [ FunctionExport {internalName = f, externalName = f}
         | f <- ["hs_init"]
       ] <> [ FunctionExport
      { internalName = "stg_returnToSchedNotPaused",
        externalName = "stg_returnToSchedNotPaused"
      }
  ]

emitErrorMessage :: [ValueType] -> BS.ByteString -> Expression
emitErrorMessage vts ev = Barf {barfMessage = ev, barfReturnTypes = vts}

floatCBits :: [(EntitySymbol, (FunctionImport, Function))]
floatCBits =
  map
    ( \(func_sym, param_vts, ret_vts) ->
        ( func_sym,
          generateRTSWrapper "floatCBits" func_sym param_vts ret_vts
        )
    )
    [ ("isFloatNegativeZero", [F32], [I32]),
      ("isDoubleNegativeZero", [F64], [I32]),
      ("isFloatNaN", [F32], [I32]),
      ("isDoubleNaN", [F64], [I32]),
      ("isFloatFinite", [F32], [I32]),
      ("isDoubleFinite", [F64], [I32]),
      ("isFloatDenormalized", [F32], [I32]),
      ("isDoubleDenormalized", [F64], [I32]),
      ("isFloatInfinite", [F32], [I32]),
      ("isDoubleInfinite", [F64], [I32]),
      ("__decodeFloat_Int", [I32, I32, F32], []),
      ("rintDouble", [F64], [F64]),
      ("rintFloat", [F32], [F32]),
      ("__decodeDouble_2Int", [I32, I32, I32, I32, F64], [])
    ]

generateRTSWrapper ::
  BS.ByteString ->
  EntitySymbol ->
  [ValueType] ->
  [ValueType] ->
  (FunctionImport, Function)
generateRTSWrapper mod_sym func_sym param_vts ret_vts =
  ( FunctionImport
      { internalName = "__asterius_" <> entityName func_sym,
        externalModuleName = mod_sym,
        externalBaseName = entityName func_sym,
        functionType = FunctionType
          { paramTypes = map fst xs,
            returnTypes = fst ret
          }
      },
    Function
      { functionType = FunctionType
          { paramTypes = param_vts,
            returnTypes = ret_vts
          },
        varTypes = [],
        body = snd
          ret
          Call
            { target = "__asterius_" <> func_sym,
              operands = map snd xs,
              callReturnTypes = fst ret
            }
      }
  )
  where
    xs =
      zipWith
        ( \i vt -> (vt, GetLocal {index = i, valueType = vt})
        )
        [0 ..]
        param_vts
    ret = (ret_vts, id)

generateWrapperFunction :: EntitySymbol -> Function -> Function
generateWrapperFunction func_sym Function {functionType = FunctionType {..}} =
  Function
    { functionType = FunctionType
        { paramTypes =
            [ wrapper_param_type
              | (_, wrapper_param_type, _) <-
                  wrapper_param_types
            ],
          returnTypes = wrapper_return_types
        },
      varTypes = [],
      body = to_wrapper_return_types $ Call
        { target = func_sym,
          operands =
            [ from_wrapper_param_type GetLocal
                { index = i,
                  valueType = wrapper_param_type
                }
              | (i, wrapper_param_type, from_wrapper_param_type) <-
                  wrapper_param_types
            ],
          callReturnTypes = returnTypes
        }
    }
  where
    wrapper_param_types =
      [ (i, param_type, id)
        | (i, param_type) <- zip [0 ..] paramTypes
      ]
    (wrapper_return_types, to_wrapper_return_types) = (returnTypes, id)

-- Renames each function in the module to <name>_wrapper, and
-- edits their implementation using 'generateWrapperFunction'
generateWrapperModule :: AsteriusModule -> AsteriusModule
generateWrapperModule m =
  m
    { functionMap = SM.fromList $ map wrap $ SM.toList $ functionMap m
    }
  where
    wrap (n, f) = (n <> "_wrapper", generateWrapperFunction n f)

initCapability :: EDSL ()
initCapability = do
  storeI32 mainCapability offset_Capability_no $ constI32 0
  storeI32 mainCapability offset_Capability_node $ constI32 0
  storeI32 mainCapability offset_Capability_idle $ constI32 0
  storeI8 mainCapability offset_Capability_disabled $ constI32 0
  storeI32 mainCapability offset_Capability_total_allocated $ constI32 0
  storeI32
    mainCapability
    (offset_Capability_f + offset_StgFunTable_stgEagerBlackholeInfo)
    $ symbol "__stg_EAGER_BLACKHOLE_info"
  storeI32 mainCapability (offset_Capability_f + offset_StgFunTable_stgGCEnter1) $
    symbol "__stg_gc_enter_1"
  storeI32 mainCapability (offset_Capability_f + offset_StgFunTable_stgGCFun) $
    symbol "__stg_gc_fun"
  storeI32 mainCapability offset_Capability_weak_ptr_list_hd $ constI32 0
  storeI32 mainCapability offset_Capability_weak_ptr_list_tl $ constI32 0
  storeI32 mainCapability offset_Capability_context_switch $ constI32 0
  storeI32 mainCapability (offset_Capability_r + offset_StgRegTable_rCCCS) $
    constI32 0
  storeI32 mainCapability (offset_Capability_r + offset_StgRegTable_rCurrentTSO) $
    constI32 0

hsInitFunction :: BuiltinsOptions -> AsteriusModule
hsInitFunction _ = runEDSL "hs_init" $ do
  initCapability
  bd_nursery <- call' "__asterius_hpAlloc" [constI32 4] I32
  putLVal currentNursery bd_nursery

rtsApplyFunction :: BuiltinsOptions -> AsteriusModule
rtsApplyFunction _ = runEDSL "rts_apply" $ do
  setReturnTypes [I32]
  [f, arg] <- params [I32, I32]
  ap <-
    call'
      "allocate"
      [mainCapability, constI32 $ roundup_bytes_to_words sizeof_StgThunk + 2]
      I32
  storeI32 ap 0 $ symbol "stg_ap_2_upd_info"
  storeI32 ap offset_StgThunk_payload f
  storeI32 ap (offset_StgThunk_payload + 4) arg
  emit ap

rtsGetSchedStatusFunction :: BuiltinsOptions -> AsteriusModule
rtsGetSchedStatusFunction _ = runEDSL "rts_getSchedStatus" $ do
  setReturnTypes [I32]
  tid <- param I32
  call' "__asterius_getTSOrstat" [tid] I32 >>= emit

rtsCheckSchedStatusFunction :: BuiltinsOptions -> AsteriusModule
rtsCheckSchedStatusFunction _ = runEDSL "rts_checkSchedStatus" $ do
  tid <- param I32
  stat <- call' "rts_getSchedStatus" [tid] I32
  if' [] (stat `eqInt32` constI32 scheduler_Success) mempty
    $ emit
    $ emitErrorMessage [] "IllegalSchedulerStatusCode"

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

-- `_scheduleTSO(tso,func)` executes the given tso starting at the given
-- function
scheduleTSOFunction :: BuiltinsOptions -> AsteriusModule
scheduleTSOFunction BuiltinsOptions {} = runEDSL "scheduleTSO" $ do
  tso <- param I32
  -- store the current TSO
  putLVal currentTSO tso
  -- indicate in the Capability that we are running the TSO
  -- TODO: remove all the useless Capability related stuff
  storeI32
    mainCapability
    (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
    tso
  storeI32 mainCapability offset_Capability_interrupt $ constI32 0
  storeI32 mainCapability offset_Capability_idle $ constI32 0
  dirtyTSO mainCapability tso
  dirtySTACK mainCapability (loadI32 tso offset_StgTSO_stackobj)
  -- execute the TSO (using stgRun trampolining machinery)
  stgRun $ symbol "stg_returnToStackTop"
  -- indicate in the Capability that we are not running anything
  storeI32
    mainCapability
    (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
    (constI32 0)
  storeI32 mainCapability offset_Capability_interrupt $ constI32 1
  storeI32 mainCapability offset_Capability_idle $ constI32 1
  -- unset the current TSO
  putLVal currentTSO (constI32 0)

-- Return the thread ID of the given tso
getThreadIdFunction :: BuiltinsOptions -> AsteriusModule
getThreadIdFunction BuiltinsOptions {} = runEDSL "rts_getThreadId" $ do
  setReturnTypes [I32]
  tso <- param I32
  emit (loadI32 tso offset_StgTSO_id)

createThreadFunction :: BuiltinsOptions -> AsteriusModule
createThreadFunction BuiltinsOptions {..} = runEDSL "createThread" $ do
  setReturnTypes [I32]
  tso_p <-
    call'
      "allocatePinned"
      [mainCapability, constI32 $ roundup_bytes_to_words sizeof_StgTSO]
      I32
  stack_p <- call' "allocatePinned" [mainCapability, constI32 4096] I32
  storeI32 stack_p 0 $ symbol "stg_STACK_info"
  stack_size_w <- local I32 $ constI32 $ (4096 - offset_StgStack_stack) `div` 4
  storeI32 stack_p offset_StgStack_stack_size stack_size_w
  storeI32 stack_p offset_StgStack_sp $
    (stack_p `addInt32` constI32 offset_StgStack_stack)
      `addInt32` (stack_size_w `mulInt32` constI32 4)
  storeI32 stack_p offset_StgStack_dirty $ constI32 1
  storeI32 tso_p 0 $ symbol "stg_TSO_info"
  storeI16 tso_p offset_StgTSO_what_next $ constI32 next_ThreadRunGHC
  storeI16 tso_p offset_StgTSO_why_blocked $ constI32 blocked_NotBlocked
  storeI32 tso_p offset_StgTSO_blocked_exceptions $
    symbol "stg_END_TSO_QUEUE_closure"
  storeI32 tso_p offset_StgTSO_flags $ constI32 0
  storeI32 tso_p offset_StgTSO_dirty $ constI32 1
  storeI32 tso_p offset_StgTSO_saved_errno $ constI32 0
  storeI32 tso_p offset_StgTSO_cap mainCapability
  storeI32 tso_p offset_StgTSO_stackobj stack_p
  storeI32 tso_p offset_StgTSO_tot_stack_size stack_size_w
  storeI64 tso_p offset_StgTSO_alloc_limit (constI64 0)
  storeI32 stack_p offset_StgStack_sp $
    loadI32 stack_p offset_StgStack_sp
      `subInt32` constI32 (4 * roundup_bytes_to_words sizeof_StgStopFrame)
  storeI32 (loadI32 stack_p offset_StgStack_sp) 0 $
    symbol "stg_stop_thread_info"
  call' "__asterius_newTSO" [] I32 >>= storeI32 tso_p offset_StgTSO_id
  emit tso_p

pushClosure :: Expression -> Expression -> EDSL ()
pushClosure tso c = do
  stack_p <- local I32 $ loadI32 tso offset_StgTSO_stackobj
  storeI32 stack_p offset_StgStack_sp $
    loadI32 stack_p offset_StgStack_sp
      `subInt32` constI32 4
  storeI32 (loadI32 stack_p offset_StgStack_sp) 0 c

createThreadHelper :: (Expression -> [Expression]) -> EDSL ()
createThreadHelper mk_closures = do
  setReturnTypes [I32]
  closure <- param I32
  t <- call' "createThread" [] I32
  for_ (mk_closures closure) $ pushClosure t
  emit t

createIOThreadFunction :: BuiltinsOptions -> AsteriusModule
createIOThreadFunction _ =
  runEDSL "createIOThread" $ createThreadHelper $ \closure ->
    [symbol "stg_ap_v_info", closure, symbol "stg_enter_info"]

createStrictIOThreadFunction :: BuiltinsOptions -> AsteriusModule
createStrictIOThreadFunction _ =
  runEDSL "createStrictIOThread" $ createThreadHelper $ \closure ->
    [ symbol "stg_forceIO_info",
      symbol "stg_ap_v_info",
      closure,
      symbol "stg_enter_info"
    ]

genAllocateFunction ::
  BuiltinsOptions ->
  -- Name of the allocation function
  EntitySymbol ->
  -- Module representing the function
  AsteriusModule
genAllocateFunction (BuiltinsOptions {}) n = runEDSL n $ do
  setReturnTypes [I32]
  [_, m] <- params [I32, I32]
  r <- call' "__asterius_allocate" [m] I32
  emit r

{-
allocateFunction BuiltinsOptions {} =
  runEDSL "allocate" $ do
    setReturnTypes [I64]
    [_, n] <- params [I64, I64]
    (truncUFloat64ToInt64 <$>
     call' "__asterius_allocate" [convertUInt64ToFloat64 n] F64) >>=
      emit
  -}
allocatePinnedFunction :: BuiltinsOptions -> AsteriusModule
allocatePinnedFunction _ = runEDSL "allocatePinned" $ do
  setReturnTypes [I32]
  [_, n] <- params [I32, I32]
  r <- call' "__asterius_allocatePinned" [n] I32
  emit r

newCAFFunction :: BuiltinsOptions -> AsteriusModule
newCAFFunction _ = runEDSL "newCAF" $ do
  setReturnTypes [I32]
  [reg, caf] <- params [I32, I32]
  orig_info <- local I32 $ loadI32 caf 0
  storeI32 caf offset_StgIndStatic_saved_info orig_info
  bh <-
    call'
      "allocate"
      [mainCapability, constI32 $ roundup_bytes_to_words sizeof_StgInd]
      I32
  storeI32 bh 0 $ symbol "stg_CAF_BLACKHOLE_info"
  storeI32 bh offset_StgInd_indirectee $
    loadI32 reg offset_StgRegTable_rCurrentTSO
  storeI32 caf offset_StgIndStatic_indirectee bh
  storeI32 caf 0 $ symbol "stg_IND_STATIC_info"
  emit bh

-- Repeatedly calls the function pointed by ``__asterius_pc`` until this
-- pointer is NULL.
--
-- This trampolining code is required to implement indirect tail-calls: instead
-- of jumping to a computed function as a tail call (which is not possible in
-- Wasm), we return the computed function into ``__asterius_pc`` and the loop in
-- ``stgRun`` performs the call.
stgRun :: Expression -> EDSL ()
stgRun init_f = do
  pc_reg <- mutLocal I32
  storeI32 (symbol "__asterius_pc") 0 init_f
  loop' [] $ \loop_lbl -> do
    putLVal pc_reg $ loadI32 (symbol "__asterius_pc") 0
    if' [] (eqZInt32 (getLVal pc_reg)) mempty $ do
      callIndirect (getLVal pc_reg)
      break' loop_lbl Nothing

-- Return from a STG function
stgReturnFunction :: BuiltinsOptions -> AsteriusModule
stgReturnFunction _ =
  runEDSL "StgReturn" $ storeI32 (symbol "__asterius_pc") 0 (constI32 0) -- store NULL in the __asterius_pc register. This will break stgRun
      -- trampolining loop.

getStablePtrWrapperFunction :: BuiltinsOptions -> AsteriusModule
getStablePtrWrapperFunction _ = runEDSL "getStablePtr" $ do
  setReturnTypes [I32]
  obj32 <- param I32
  sp_i32 <-
    call'
      "__asterius_newStablePtr"
      [obj32]
      I32
  emit sp_i32

deRefStablePtrWrapperFunction :: BuiltinsOptions -> AsteriusModule
deRefStablePtrWrapperFunction _ = runEDSL "deRefStablePtr" $ do
  setReturnTypes [I32]
  sp32 <- param I32
  obj_i32 <-
    call'
      "__asterius_deRefStablePtr"
      [sp32]
      I32
  emit obj_i32

freeStablePtrWrapperFunction :: BuiltinsOptions -> AsteriusModule
freeStablePtrWrapperFunction _ = runEDSL "hs_free_stable_ptr" $ do
  sp32 <- param I32
  call "__asterius_freeStablePtr" [sp32]

makeStableNameWrapperFunction :: BuiltinsOptions -> AsteriusModule
makeStableNameWrapperFunction _ = runEDSL "makeStableName" $ do
  setReturnTypes [I32]
  sp32 <- param I32
  obj_i32 <-
    call'
      "__asterius_makeStableName"
      [sp32]
      I32
  emit obj_i32

rtsMkHelper ::
  BuiltinsOptions ->
  -- Name of the function to be built
  EntitySymbol ->
  -- Mangled name of the primop constructor
  EntitySymbol ->
  AsteriusModule
rtsMkHelper _ n con_sym = runEDSL n $ do
  setReturnTypes [I32]
  [i] <- params [I32]
  p <- call' "allocate" [mainCapability, constI32 2] I32
  storeI32 p 0 $ symbol con_sym
  storeI32 p 4 i
  emit p

rtsMkBoolFunction :: BuiltinsOptions -> AsteriusModule
rtsMkBoolFunction _ = runEDSL "rts_mkBool" $ do
  setReturnTypes [I32]
  [i] <- params [I32]
  if'
    [I32]
    (eqZInt32 i)
    (emit $ symbol "ghczmprim_GHCziTypes_False_closure")
    (emit $ symbol "ghczmprim_GHCziTypes_True_closure")

rtsMkDoubleFunction :: BuiltinsOptions -> AsteriusModule
rtsMkDoubleFunction _ = runEDSL "rts_mkDouble" $ do
  setReturnTypes [I32]
  [i] <- params [F64]
  p <- call' "allocate" [mainCapability, constI32 3] I32
  storeI32 p 0 $ symbol "ghczmprim_GHCziTypes_Dzh_con_info"
  storeF64 p 4 i
  emit p

rtsMkCharFunction :: BuiltinsOptions -> AsteriusModule
rtsMkCharFunction _ = runEDSL "rts_mkChar" $ do
  setReturnTypes [I32]
  [i] <- params [I32]
  p <- call' "allocate" [mainCapability, constI32 2] I32
  storeI32 p 0 $ symbol "ghczmprim_GHCziTypes_Czh_con_info"
  storeI32 p 4 i
  emit p

rtsMkIntFunction :: BuiltinsOptions -> AsteriusModule
rtsMkIntFunction _ = runEDSL "rts_mkInt" $ do
  setReturnTypes [I32]
  [i] <- params [I32]
  p <- call' "allocate" [mainCapability, constI32 2] I32
  storeI32 p 0 $ symbol "ghczmprim_GHCziTypes_Izh_con_info"
  storeI32 p 4 i
  emit p

rtsMkWordFunction :: BuiltinsOptions -> AsteriusModule
rtsMkWordFunction opts =
  rtsMkHelper opts "rts_mkWord" "ghczmprim_GHCziTypes_Wzh_con_info"

rtsMkPtrFunction :: BuiltinsOptions -> AsteriusModule
rtsMkPtrFunction opts =
  rtsMkHelper opts "rts_mkPtr" "base_GHCziPtr_Ptr_con_info"

rtsMkStablePtrFunction :: BuiltinsOptions -> AsteriusModule
rtsMkStablePtrFunction opts =
  rtsMkHelper opts "rts_mkStablePtr" "base_GHCziStable_StablePtr_con_info"

rtsMkJSValFunction :: BuiltinsOptions -> AsteriusModule
rtsMkJSValFunction opts =
  rtsMkHelper opts "rts_mkJSVal" "base_AsteriusziTypesziJSVal_JSVal_con_info"

rtsGetBoolFunction :: BuiltinsOptions -> AsteriusModule
rtsGetBoolFunction _ = runEDSL "rts_getBool" $ do
  setReturnTypes [I32]
  p <- param I32
  emit $ neInt32
      (constI32 0)
      (loadI32 (loadI32 (unTagClosure p) 0) offset_StgInfoTable_srt)

rtsGetDoubleFunction :: BuiltinsOptions -> AsteriusModule
rtsGetDoubleFunction _ = runEDSL "rts_getDouble" $ do
  setReturnTypes [F64]
  p <- param I32
  emit $ loadF64 (unTagClosure p) offset_StgClosure_payload

-- rtsGetCharFunction = rtsGetIntFunction
-- generate a function which internally performs getInt, but is
-- named differently
generateRtsGetIntFunction ::
  BuiltinsOptions ->
  EntitySymbol -> -- Name of the function
  AsteriusModule
generateRtsGetIntFunction _ n = runEDSL n $ do
  setReturnTypes [I32]
  p <- param I32
  emit $ loadI32 (unTagClosure p) offset_StgClosure_payload

{-
rtsGetIntFunction _ =
  runEDSL "rts_getInt" $ do
    setReturnTypes [I64]
    p <- param I64
    emit $ loadI64 (unTagClosure p) offset_StgClosure_payload
-}
loadI64Function :: BuiltinsOptions -> AsteriusModule
loadI64Function _ = runEDSL "loadI64" $ do
  setReturnTypes [I64]
  p <- param I32
  emit $ loadI64 p 0

strlenFunction :: BuiltinsOptions -> AsteriusModule
strlenFunction _ = runEDSL "strlen" $ do
  setReturnTypes [I32]
  [str] <- params [I32]
  len <- call' "__asterius_strlen" [str] I32
  emit len

memchrFunction :: BuiltinsOptions -> AsteriusModule
memchrFunction _ = runEDSL "memchr" $ do
  setReturnTypes [I32]
  [ptr, val, num] <- params [I32, I32, I32]
  p <-
    call'
      "__asterius_memchr"
      [ptr, val, num]
      I32
  emit p

threadPausedFunction :: BuiltinsOptions -> AsteriusModule
threadPausedFunction _ = runEDSL "threadPaused" $ do
  _ <- params [I32, I32]
  pure ()

dirtyMutVarFunction :: BuiltinsOptions -> AsteriusModule
dirtyMutVarFunction _ = runEDSL "dirty_MUT_VAR" $ do
  [_, p, _] <- params [I32, I32, I32]
  if'
    []
    (loadI32 p 0 `eqInt32` symbol "stg_MUT_VAR_CLEAN_info")
    (storeI32 p 0 $ symbol "stg_MUT_VAR_DIRTY_info")
    mempty

dirtyMVarFunction :: BuiltinsOptions -> AsteriusModule
dirtyMVarFunction _ = runEDSL "dirty_MVAR" $ do
  [_basereg, _mvar, _] <- params [I32, I32, I32]
  mempty

dirtyStackFunction :: BuiltinsOptions -> AsteriusModule
dirtyStackFunction _ = runEDSL "dirty_STACK" $ do
  [cap, stack] <- params [I32, I32]
  dirtySTACK cap stack

recordClosureMutatedFunction :: BuiltinsOptions -> AsteriusModule
recordClosureMutatedFunction _ = runEDSL "recordClosureMutated" $ do
  [_cap, _closure] <- params [I32, I32]
  mempty

tryWakeupThreadFunction :: BuiltinsOptions -> AsteriusModule
tryWakeupThreadFunction _ = runEDSL "tryWakeupThread" $ do
  [_cap, tso] <- params [I32, I32]
  call "__asterius_enqueueTSO" [tso]

raiseExceptionHelperFunction :: BuiltinsOptions -> AsteriusModule
raiseExceptionHelperFunction _ = runEDSL "raiseExceptionHelper" $ do
  setReturnTypes [I32]
  args <- params [I32, I32, I32]
  frame_type <-
    call'
        "__asterius_raiseExceptionHelper"
        args
        I32
  emit frame_type

-- Note that generateRTSWrapper will treat all our numbers as signed, not
-- unsigned.   This is OK for ASCII code, since the ints we have will not be
-- larger than 2^15.  However, for larger numbers, it would "overflow", and
-- would treat large unsigned  numbers as negative signed numbers.
unicodeCBits :: [(EntitySymbol, (FunctionImport, Function))]
unicodeCBits =
  map
    ( \(func_sym, param_vts, ret_vts) ->
        ( func_sym,
          generateRTSWrapper "Unicode" func_sym param_vts ret_vts
        )
    )
    [ ("u_gencat", [I32], [I32]),
      ("u_iswalpha", [I32], [I32]),
      ("u_iswalnum", [I32], [I32]),
      ("u_iswupper", [I32], [I32]),
      ("u_iswlower", [I32], [I32]),
      ("u_towlower", [I32], [I32]),
      ("u_towupper", [I32], [I32]),
      ("u_towtitle", [I32], [I32]),
      ("u_iswcntrl", [I32], [I32]),
      ("u_iswprint", [I32], [I32])
    ]

suspendThreadFunction :: BuiltinsOptions -> AsteriusModule
suspendThreadFunction _ = runEDSL "suspendThread" $ do
  setReturnTypes [I32]
  [reg, _] <- params [I32, I32]
  emit reg

scheduleThreadFunction :: BuiltinsOptions -> AsteriusModule
scheduleThreadFunction _ = runEDSL "scheduleThread" $ do
  setReturnTypes []
  [_cap, tso] <- params [I32, I32]
  call "__asterius_enqueueTSO" [tso]

scheduleThreadOnFunction :: BuiltinsOptions -> AsteriusModule
scheduleThreadOnFunction _ = runEDSL "scheduleThreadOn" $ do
  setReturnTypes []
  [_cap, _cpu, tso] <- params [I32, I32, I32]
  call "__asterius_enqueueTSO" [tso]

resumeThreadFunction :: BuiltinsOptions -> AsteriusModule
resumeThreadFunction _ = runEDSL "resumeThread" $ do
  setReturnTypes [I32]
  reg <- param I32
  emit reg

performMajorGCFunction :: BuiltinsOptions -> AsteriusModule
performMajorGCFunction _ =
  runEDSL "performMajorGC" $ call "__asterius_performGC" []

performGCFunction :: BuiltinsOptions -> AsteriusModule
performGCFunction _ = runEDSL "performGC" $ call "performMajorGC" []

localeEncodingFunction :: BuiltinsOptions -> AsteriusModule
localeEncodingFunction _ = runEDSL "localeEncoding" $ do
  setReturnTypes [I32]
  emit $ symbol "__asterius_localeEncoding"

isattyFunction :: BuiltinsOptions -> AsteriusModule
isattyFunction _ = runEDSL "isatty" $ do
  setReturnTypes [I32]
  _ <- param I32
  emit $ constI32 0

fdReadyFunction :: BuiltinsOptions -> AsteriusModule
fdReadyFunction _ = runEDSL "fdReady" $ do
  setReturnTypes [I32]
  _ <- params [I32, I32, I64, I32]
  emit $ constI32 1

rtsSupportsBoundThreadsFunction :: BuiltinsOptions -> AsteriusModule
rtsSupportsBoundThreadsFunction _ = runEDSL "rtsSupportsBoundThreads" $ do
  setReturnTypes [I32]
  emit $ constI32 0

readFunction :: BuiltinsOptions -> AsteriusModule
readFunction _ =
  runEDSL "ghczuwrapperZC22ZCbaseZCSystemziPosixziInternalsZCread" $ do
    setReturnTypes [I32]
    [fd, buf, count] <- params [I32, I32, I32]
    r <-
      call'
          "__asterius_read"
          [fd, buf, count]
          I32
    emit r

writeFunction :: BuiltinsOptions -> AsteriusModule
writeFunction _ =
  runEDSL "ghczuwrapperZC20ZCbaseZCSystemziPosixziInternalsZCwrite" $ do
    setReturnTypes [I32]
    [fd, buf, count] <- params [I32, I32, I32]
    r <-
      call'
          "__asterius_write"
          [fd, buf, count]
          I32
    emit r
