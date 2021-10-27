{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Builtins
  ( BuiltinsOptions (..),
    defaultBuiltinsOptions,
    rtsAsteriusModule,
    rtsFunctionImports,
    rtsFunctionExports,
    rtsGlobalImports,
    rtsGlobalExports,
    emitErrorMessage,
    generateWrapperFunction,
    ShouldSext (..),
    genWrap,
    genExtend,
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
import Asterius.Builtins.SPT
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
                { staticsType = Bytes,
                  asteriusStatics =
                    [ Serialized $ BS.pack $
                        replicate
                          (8 * roundup_bytes_to_words sizeof_Capability)
                          0
                    ]
                }
            ),
            ( "rts_stop_on_exception",
              AsteriusStatics
                { staticsType = Bytes,
                  asteriusStatics = [Serialized $ encodeStorable (0 :: Word64)]
                }
            ),
            ( "n_capabilities",
              AsteriusStatics
                { staticsType = ConstBytes,
                  asteriusStatics = [Serialized $ encodeStorable (1 :: Word32)]
                }
            ),
            ( "enabled_capabilities",
              AsteriusStatics
                { staticsType = ConstBytes,
                  asteriusStatics = [Serialized $ encodeStorable (1 :: Word32)]
                }
            ),
            ( "prog_name",
              AsteriusStatics
                { staticsType = ConstBytes,
                  asteriusStatics =
                    [ Serialized $
                        fromString (progName opts <> "\0")
                    ]
                }
            ),
            ( "__asterius_localeEncoding",
              AsteriusStatics
                { staticsType = ConstBytes,
                  asteriusStatics = [Serialized "UTF-8\0"]
                }
            ),
            ( "__asterius_i64_slot",
              AsteriusStatics
                { staticsType = Bytes,
                  asteriusStatics = [Serialized $ BS.pack $ replicate 8 0]
                }
            )
          ],
      globalsMap =
        SM.fromList $
          [ ( "__asterius_pc",
              Global
                { globalType = GlobalType
                    { globalValueType = I64,
                      globalMutability = Mutable
                    },
                  globalInit = ConstI64 invalidAddress
                }
            )
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
    <> printI64Function opts
    <> printF32Function opts
    <> printF64Function opts
    <> assertEqI64Function opts
    <> strlenFunction opts
    <> debugBelch2Function opts
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
    <> (if debug opts then generateRtsAsteriusDebugModule opts else mempty)
    -- Add in the module that contain functions which need to be
    -- exposed to the outside world. So add in the module, and
    -- the module wrapped by using `generateWrapperModule`.
    <> generateRtsExternalInterfaceModule opts
    <> generateWrapperModule (generateRtsExternalInterfaceModule opts)
    <> blackholeCBits
    <> generateWrapperModule blackholeCBits
    <> exportsCBits
    <> smCBits
    <> generateWrapperModule smCBits
    <> sparksCBits
    <> schedulerCBits
    <> cmathCBits
    <> envCBits
    <> posixCBits
    <> sptCBits
    <> stgPrimFloatCBits
    <> timeCBits
    <> primitiveCBits
    <> endiannessCBits
    <> barfCBits

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

-- Generate the module consisting of debug functions
generateRtsAsteriusDebugModule :: BuiltinsOptions -> AsteriusModule
generateRtsAsteriusDebugModule opts =
  mempty
    <> getF64GlobalRegFunction opts "__asterius_Load_Sp" Sp
    <> getF64GlobalRegFunction opts "__asterius_Load_SpLim" SpLim
    <> getF64GlobalRegFunction opts "__asterius_Load_Hp" Hp
    <> getF64GlobalRegFunction opts "__asterius_Load_HpLim" SpLim

rtsFunctionImports :: Bool -> [FunctionImport]
rtsFunctionImports debug =
  [ FunctionImport
           { internalName = "__asterius_newStablePtr",
             externalModuleName = "StablePtr",
             externalBaseName = "newStablePtr",
             functionType = FunctionType
               { paramTypes = [F64],
                 returnTypes = [F64]
               }
           },
         FunctionImport
           { internalName = "__asterius_deRefStablePtr",
             externalModuleName = "StablePtr",
             externalBaseName = "deRefStablePtr",
             functionType = FunctionType
               { paramTypes = [F64],
                 returnTypes = [F64]
               }
           },
         FunctionImport
           { internalName = "__asterius_freeStablePtr",
             externalModuleName = "StablePtr",
             externalBaseName = "freeStablePtr",
             functionType = FunctionType {paramTypes = [F64], returnTypes = []}
           },
         FunctionImport
           { internalName = "__asterius_makeStableName",
             externalModuleName = "StableName",
             externalBaseName = "makeStableName",
             functionType = FunctionType
               { paramTypes = [F64],
                 returnTypes = [F64]
               }
           },
         FunctionImport
           { internalName = "printI64",
             externalModuleName = "rts",
             externalBaseName = "printI64",
             functionType = FunctionType {paramTypes = [F64], returnTypes = []}
           },
         FunctionImport
           { internalName = "assertEqI64",
             externalModuleName = "rts",
             externalBaseName = "assertEqI64",
             functionType = FunctionType
               { paramTypes = [F64, F64],
                 returnTypes = []
               }
           },
         FunctionImport
           { internalName = "printF32",
             externalModuleName = "rts",
             externalBaseName = "print",
             functionType = FunctionType {paramTypes = [F32], returnTypes = []}
           },
         FunctionImport
           { internalName = "printF64",
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
               { paramTypes = [F64],
                 returnTypes = [F64]
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
               { paramTypes = [F64],
                 returnTypes = [F64]
               }
           },
         FunctionImport
           { internalName = "__asterius_debugBelch2",
             externalModuleName = "Messages",
             externalBaseName = "debugBelch2",
             functionType = FunctionType
               { paramTypes = [F64, F64],
                 returnTypes = []
               }
           },
         FunctionImport
           { internalName = "__asterius_memchr",
             externalModuleName = "Memory",
             externalBaseName = "memchr",
             functionType = FunctionType
               { paramTypes = [F64, F64, F64],
                 returnTypes = [F64]
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
               { paramTypes = [F64, F64, F64],
                 returnTypes = [F64]
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
               { paramTypes = [F64, F64, F64],
                 returnTypes = [F64]
               }
           },
         FunctionImport
           { internalName = "__asterius_write",
             externalModuleName = "fs",
             externalBaseName = "write",
             functionType = FunctionType
               { paramTypes = [F64, F64, F64],
                 returnTypes = [F64]
               }
           }
       ]
    <> ( if debug
           then
             [ FunctionImport
                 { internalName = "__asterius_traceCmm",
                   externalModuleName = "Tracing",
                   externalBaseName = "traceCmm",
                   functionType = FunctionType
                     { paramTypes = [F64],
                       returnTypes = []
                     }
                 },
               FunctionImport
                 { internalName = "__asterius_traceCmmBlock",
                   externalModuleName = "Tracing",
                   externalBaseName = "traceCmmBlock",
                   functionType = FunctionType
                     { paramTypes = [F64, I32],
                       returnTypes = []
                     }
                 },
               FunctionImport
                 { internalName = "__asterius_traceCmmSetLocal",
                   externalModuleName = "Tracing",
                   externalBaseName = "traceCmmSetLocal",
                   functionType = FunctionType
                     { paramTypes = [F64, I32, F64],
                       returnTypes = []
                     }
                 }
             ]
               <> concat
                 [ [ FunctionImport
                       { internalName = "__asterius_load_" <> k,
                         externalModuleName = "MemoryTrap",
                         externalBaseName = "load" <> k,
                         functionType = FunctionType
                           { paramTypes = [I64, I64, I32],
                             returnTypes = [t]
                           }
                       },
                     FunctionImport
                       { internalName = "__asterius_store_" <> k,
                         externalModuleName = "MemoryTrap",
                         externalBaseName = "store" <> k,
                         functionType = FunctionType
                           { paramTypes = [I64, I64, I32, t],
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
                          { paramTypes = [I64, I64, I32],
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
    <> sptImports
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
         | f <-
             ( if debug
                 then
                   [ "__asterius_Load_Sp",
                     "__asterius_Load_SpLim",
                     "__asterius_Load_Hp",
                     "__asterius_Load_HpLim"
                   ]
                 else []
             )
               <> ["hs_init"]
       ] <> [ FunctionExport
      { internalName = "stg_returnToSchedNotPaused",
        externalName = "stg_returnToSchedNotPaused"
      }
  ]

rtsGlobalImports :: [GlobalImport]
rtsGlobalImports =
  [ GlobalImport
      { internalName = "__asterius_memory_base",
        externalModuleName = "env",
        externalBaseName = "__memory_base",
        globalType = GlobalType
          { globalValueType = I32,
            globalMutability = Immutable
          }
      },
    GlobalImport
      { internalName = "__asterius_table_base",
        externalModuleName = "env",
        externalBaseName = "__table_base",
        globalType = GlobalType
          { globalValueType = I32,
            globalMutability = Immutable
          }
      }
  ]

rtsGlobalExports :: [GlobalExport]
rtsGlobalExports = mempty

emitErrorMessage :: [ValueType] -> BS.ByteString -> Expression
emitErrorMessage vts ev = Barf {barfMessage = ev, barfReturnTypes = vts}

floatCBits :: [(EntitySymbol, (FunctionImport, Function))]
floatCBits =
  map
    ( \(func_sym, param_vts, ret_vts) ->
        ( mkEntitySymbol func_sym,
          generateRTSWrapper "floatCBits" func_sym param_vts ret_vts
        )
    )
    [ ("isFloatNegativeZero", [F32], [I64]),
      ("isDoubleNegativeZero", [F64], [I64]),
      ("isFloatNaN", [F32], [I64]),
      ("isDoubleNaN", [F64], [I64]),
      ("isFloatFinite", [F32], [I64]),
      ("isDoubleFinite", [F64], [I64]),
      ("isFloatDenormalized", [F32], [I64]),
      ("isDoubleDenormalized", [F64], [I64]),
      ("isFloatInfinite", [F32], [I64]),
      ("isDoubleInfinite", [F64], [I64]),
      ("__decodeFloat_Int", [I64, I64, F32], []),
      ("rintDouble", [F64], [F64]),
      ("rintFloat", [F32], [F32]),
      ("__decodeDouble_2Int", [I64, I64, I64, I64, F64], [])
    ]

generateRTSWrapper ::
  BS.ByteString ->
  BS.ByteString ->
  [ValueType] ->
  [ValueType] ->
  (FunctionImport, Function)
generateRTSWrapper mod_sym func_sym param_vts ret_vts =
  ( FunctionImport
      { internalName = "__asterius_" <> func_sym,
        externalModuleName = mod_sym,
        externalBaseName = func_sym,
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
          CallImport
            { target' = "__asterius_" <> func_sym,
              operands = map snd xs,
              callImportReturnTypes = fst ret
            }
      }
  )
  where
    xs =
      zipWith
        ( \i vt -> case vt of
            I64 ->
              (F64, convertSInt64ToFloat64 GetLocal {index = i, valueType = I64})
            _ -> (vt, GetLocal {index = i, valueType = vt})
        )
        [0 ..]
        param_vts
    ret = case ret_vts of
      [I64] -> ([F64], truncSFloat64ToInt64)
      _ -> (ret_vts, id)

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
          callReturnTypes = returnTypes,
          callHint = Nothing
        }
    }
  where
    wrapper_param_types =
      [ case param_type of
          I64 -> (i, F64, truncSFloat64ToInt64)
          _ -> (i, param_type, id)
        | (i, param_type) <- zip [0 ..] paramTypes
      ]
    (wrapper_return_types, to_wrapper_return_types) = case returnTypes of
      [I64] -> ([F64], convertSInt64ToFloat64)
      _ -> (returnTypes, id)

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
  storeI64 mainCapability offset_Capability_total_allocated $ constI64 0
  storeI64
    mainCapability
    (offset_Capability_f + offset_StgFunTable_stgEagerBlackholeInfo)
    $ symbol "__stg_EAGER_BLACKHOLE_info"
  storeI64 mainCapability (offset_Capability_f + offset_StgFunTable_stgGCEnter1) $
    symbol "__stg_gc_enter_1"
  storeI64 mainCapability (offset_Capability_f + offset_StgFunTable_stgGCFun) $
    symbol "__stg_gc_fun"
  storeI64 mainCapability offset_Capability_weak_ptr_list_hd $ constI64 0
  storeI64 mainCapability offset_Capability_weak_ptr_list_tl $ constI64 0
  storeI32 mainCapability offset_Capability_context_switch $ constI32 0
  storeI64 mainCapability (offset_Capability_r + offset_StgRegTable_rCCCS) $
    constI64 0
  storeI64 mainCapability (offset_Capability_r + offset_StgRegTable_rCurrentTSO) $
    constI64 0

hsInitFunction :: BuiltinsOptions -> AsteriusModule
hsInitFunction _ = runEDSL "hs_init" $ do
  initCapability
  bd_nursery <-
    truncUFloat64ToInt64 <$> callImport' "__asterius_hpAlloc" [constF64 8] F64
  putLVal currentNursery bd_nursery

rtsApplyFunction :: BuiltinsOptions -> AsteriusModule
rtsApplyFunction _ = runEDSL "rts_apply" $ do
  setReturnTypes [I64]
  [f, arg] <- params [I64, I64]
  ap <-
    call'
      "allocate"
      [mainCapability, constI64 $ roundup_bytes_to_words sizeof_StgThunk + 2]
      I64
  storeI64 ap 0 $ symbol "stg_ap_2_upd_info"
  storeI64 ap offset_StgThunk_payload f
  storeI64 ap (offset_StgThunk_payload + 8) arg
  emit ap

rtsGetSchedStatusFunction :: BuiltinsOptions -> AsteriusModule
rtsGetSchedStatusFunction _ = runEDSL "rts_getSchedStatus" $ do
  setReturnTypes [I32]
  tid <- param I32
  callImport' "__asterius_getTSOrstat" [tid] I32 >>= emit

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
  tso <- param I64
  -- store the current TSO
  putLVal currentTSO tso
  -- indicate in the Capability that we are running the TSO
  -- TODO: remove all the useless Capability related stuff
  storeI64
    mainCapability
    (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
    tso
  storeI32 mainCapability offset_Capability_interrupt $ constI32 0
  storeI32 mainCapability offset_Capability_idle $ constI32 0
  dirtyTSO mainCapability tso
  dirtySTACK mainCapability (loadI64 tso offset_StgTSO_stackobj)
  -- execute the TSO (using stgRun trampolining machinery)
  stgRun $ symbol "stg_returnToStackTop"
  -- indicate in the Capability that we are not running anything
  storeI64
    mainCapability
    (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
    (constI64 0)
  storeI32 mainCapability offset_Capability_interrupt $ constI32 1
  storeI32 mainCapability offset_Capability_idle $ constI32 1
  -- unset the current TSO
  putLVal currentTSO (constI64 0)

-- Return the thread ID of the given tso
getThreadIdFunction :: BuiltinsOptions -> AsteriusModule
getThreadIdFunction BuiltinsOptions {} = runEDSL "rts_getThreadId" $ do
  setReturnTypes [I64]
  tso <- param I64
  emit (extendUInt32 (loadI32 tso offset_StgTSO_id))

createThreadFunction :: BuiltinsOptions -> AsteriusModule
createThreadFunction BuiltinsOptions {..} = runEDSL "createThread" $ do
  setReturnTypes [I64]
  tso_p <-
    call'
      "allocatePinned"
      [mainCapability, constI64 $ roundup_bytes_to_words sizeof_StgTSO]
      I64
  stack_p <- call' "allocatePinned" [mainCapability, constI64 4096] I64
  storeI64 stack_p 0 $ symbol "stg_STACK_info"
  stack_size_w <- i64Local $ constI64 $ (4096 - offset_StgStack_stack) `div` 8
  storeI32 stack_p offset_StgStack_stack_size $ wrapInt64 stack_size_w
  storeI64 stack_p offset_StgStack_sp $
    (stack_p `addInt64` constI64 offset_StgStack_stack)
      `addInt64` (stack_size_w `mulInt64` constI64 8)
  storeI32 stack_p offset_StgStack_dirty $ constI32 1
  storeI64 tso_p 0 $ symbol "stg_TSO_info"
  storeI16 tso_p offset_StgTSO_what_next $ constI32 next_ThreadRunGHC
  storeI16 tso_p offset_StgTSO_why_blocked $ constI32 blocked_NotBlocked
  storeI64 tso_p offset_StgTSO_blocked_exceptions $
    symbol "stg_END_TSO_QUEUE_closure"
  storeI32 tso_p offset_StgTSO_flags $ constI32 0
  storeI32 tso_p offset_StgTSO_dirty $ constI32 1
  storeI32 tso_p offset_StgTSO_saved_errno $ constI32 0
  storeI64 tso_p offset_StgTSO_cap mainCapability
  storeI64 tso_p offset_StgTSO_stackobj stack_p
  storeI32 tso_p offset_StgTSO_tot_stack_size $ wrapInt64 stack_size_w
  storeI64 tso_p offset_StgTSO_alloc_limit (constI64 0)
  storeI64 stack_p offset_StgStack_sp $
    loadI64 stack_p offset_StgStack_sp
      `subInt64` constI64 (8 * roundup_bytes_to_words sizeof_StgStopFrame)
  storeI64 (loadI64 stack_p offset_StgStack_sp) 0 $
    symbol "stg_stop_thread_info"
  callImport' "__asterius_newTSO" [] I32 >>= storeI32 tso_p offset_StgTSO_id
  emit tso_p

pushClosure :: Expression -> Expression -> EDSL ()
pushClosure tso c = do
  stack_p <- i64Local $ loadI64 tso offset_StgTSO_stackobj
  storeI64 stack_p offset_StgStack_sp $
    loadI64 stack_p offset_StgStack_sp
      `subInt64` constI64 8
  storeI64 (loadI64 stack_p offset_StgStack_sp) 0 c

createThreadHelper :: (Expression -> [Expression]) -> EDSL ()
createThreadHelper mk_closures = do
  setReturnTypes [I64]
  closure <- param I64
  t <- call' "createThread" [] I64
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
  setReturnTypes [I64]
  [_, m] <- params [I64, I64]
  callImport' "__asterius_allocate" [wrapInt64 m] F64
    >>= emit . extendUInt32

{-
allocateFunction BuiltinsOptions {} =
  runEDSL "allocate" $ do
    setReturnTypes [I64]
    [_, n] <- params [I64, I64]
    (truncUFloat64ToInt64 <$>
     callImport' "__asterius_allocate" [convertUInt64ToFloat64 n] F64) >>=
      emit
  -}
allocatePinnedFunction :: BuiltinsOptions -> AsteriusModule
allocatePinnedFunction _ = runEDSL "allocatePinned" $ do
  setReturnTypes [I64]
  [_, n] <- params [I64, I64]
  callImport' "__asterius_allocatePinned" [wrapInt64 n] F64
    >>= emit . extendUInt32

newCAFFunction :: BuiltinsOptions -> AsteriusModule
newCAFFunction _ = runEDSL "newCAF" $ do
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

asterius_pc_global :: LVal
asterius_pc_global =
  newGlobal
    "__asterius_pc"
    GlobalType
      { globalValueType = I64,
        globalMutability = Mutable
      }

-- Repeatedly calls the function pointed by ``__asterius_pc`` until this
-- pointer is NULL.
--
-- This trampolining code is required to implement indirect tail-calls: instead
-- of jumping to a computed function as a tail call (which is not possible in
-- Wasm), we return the computed function into ``__asterius_pc`` and the loop in
-- ``stgRun`` performs the call.
stgRun :: Expression -> EDSL ()
stgRun init_f = do
  let pc = asterius_pc_global
  pc_reg <- i64MutLocal
  putLVal pc init_f
  loop' [] $ \loop_lbl -> do
    putLVal pc_reg $ getLVal pc
    if' [] (eqZInt64 (getLVal pc_reg)) mempty $ do
      callIndirect (getLVal pc_reg)
      break' loop_lbl Nothing

-- Return from a STG function
stgReturnFunction :: BuiltinsOptions -> AsteriusModule
stgReturnFunction _ =
  runEDSL "StgReturn" $ putLVal asterius_pc_global $ constI64 0 -- store NULL in the __asterius_pc register. This will break stgRun
      -- trampolining loop.

getStablePtrWrapperFunction :: BuiltinsOptions -> AsteriusModule
getStablePtrWrapperFunction _ = runEDSL "getStablePtr" $ do
  setReturnTypes [I64]
  obj64 <- param I64
  sp_f64 <-
    callImport'
      "__asterius_newStablePtr"
      [convertUInt64ToFloat64 obj64]
      F64
  emit $ truncUFloat64ToInt64 sp_f64

deRefStablePtrWrapperFunction :: BuiltinsOptions -> AsteriusModule
deRefStablePtrWrapperFunction _ = runEDSL "deRefStablePtr" $ do
  setReturnTypes [I64]
  sp64 <- param I64
  obj_f64 <-
    callImport'
      "__asterius_deRefStablePtr"
      [convertUInt64ToFloat64 sp64]
      F64
  emit $ truncUFloat64ToInt64 obj_f64

freeStablePtrWrapperFunction :: BuiltinsOptions -> AsteriusModule
freeStablePtrWrapperFunction _ = runEDSL "hs_free_stable_ptr" $ do
  sp64 <- param I64
  callImport "__asterius_freeStablePtr" [convertUInt64ToFloat64 sp64]

makeStableNameWrapperFunction :: BuiltinsOptions -> AsteriusModule
makeStableNameWrapperFunction _ = runEDSL "makeStableName" $ do
  setReturnTypes [I64]
  sp64 <- param I64
  obj_f64 <-
    callImport'
      "__asterius_makeStableName"
      [convertUInt64ToFloat64 sp64]
      F64
  emit $ truncUFloat64ToInt64 obj_f64

rtsMkHelper ::
  BuiltinsOptions ->
  -- Name of the function to be built
  EntitySymbol ->
  -- Mangled name of the primop constructor
  EntitySymbol ->
  AsteriusModule
rtsMkHelper _ n con_sym = runEDSL n $ do
  setReturnTypes [I64]
  [i] <- params [I64]
  p <- call' "allocate" [mainCapability, constI64 2] I64
  storeI64 p 0 $ symbol con_sym
  storeI64 p 8 i
  emit p

rtsMkBoolFunction :: BuiltinsOptions -> AsteriusModule
rtsMkBoolFunction _ = runEDSL "rts_mkBool" $ do
  setReturnTypes [I64]
  [i] <- params [I64]
  if'
    [I64]
    (eqZInt64 i)
    (emit $ symbol "ghczmprim_GHCziTypes_False_closure")
    (emit $ symbol "ghczmprim_GHCziTypes_True_closure")

rtsMkDoubleFunction :: BuiltinsOptions -> AsteriusModule
rtsMkDoubleFunction _ = runEDSL "rts_mkDouble" $ do
  setReturnTypes [I64]
  [i] <- params [F64]
  p <- call' "allocate" [mainCapability, constI64 2] I64
  storeI64 p 0 $ symbol "ghczmprim_GHCziTypes_Dzh_con_info"
  storeF64 p 8 i
  emit p

rtsMkCharFunction :: BuiltinsOptions -> AsteriusModule
rtsMkCharFunction _ = runEDSL "rts_mkChar" $ do
  setReturnTypes [I64]
  [i] <- params [I64]
  if'
    [I64]
    (i `ltUInt64` constI64 256)
    -- If the character in question is in the range [0..255] we use the
    -- trick that GHC uses, and instead of generating a heap-allocated Char
    -- closure, we simply return the address of the statically allocated
    -- Char. See stg_CHARLIKE_closure in
    -- ghc-toolkit/boot-libs/rts/StgMiscClosures.cmm
    ( let offset = i `mulInt64` constI64 16
       in emit $ symbol "stg_CHARLIKE_closure" `addInt64` offset
    )
    -- Otherwise, we fall back to the more inefficient
    -- approach and generate a dynamic closure.
    $ do
      p <- call' "allocate" [mainCapability, constI64 2] I64
      storeI64 p 0 $ symbol "ghczmprim_GHCziTypes_Czh_con_info"
      storeI64 p 8 i
      emit p

rtsMkIntFunction :: BuiltinsOptions -> AsteriusModule
rtsMkIntFunction _ = runEDSL "rts_mkInt" $ do
  setReturnTypes [I64]
  [i] <- params [I64]
  if'
    [I64]
    ((i `leSInt64` constI64 16) `andInt32` (i `geSInt64` constI64 (-16)))
    -- If the integer in question is in the range [-16..16] we use the
    -- trick that GHC uses, and instead of generating a heap-allocated Int
    -- closure, we simply return the address of the statically allocated
    -- Int. See stg_INTLIKE_closure in
    -- ghc-toolkit/boot-libs/rts/StgMiscClosures.cmm
    ( let offset = (i `addInt64` constI64 16) `mulInt64` constI64 16
       in emit $ symbol "stg_INTLIKE_closure" `addInt64` offset
    )
    -- Otherwise, we fall back to the more inefficient
    -- approach and generate a dynamic closure.
    $ do
      p <- call' "allocate" [mainCapability, constI64 2] I64
      storeI64 p 0 $ symbol "ghczmprim_GHCziTypes_Izh_con_info"
      storeI64 p 8 i
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
  setReturnTypes [I64]
  p <- param I64
  emit $ extendUInt32 $
    neInt32
      (constI32 0)
      (loadI32 (loadI64 (unTagClosure p) 0) offset_StgInfoTable_srt)

rtsGetDoubleFunction :: BuiltinsOptions -> AsteriusModule
rtsGetDoubleFunction _ = runEDSL "rts_getDouble" $ do
  setReturnTypes [F64]
  p <- param I64
  emit $ loadF64 (unTagClosure p) offset_StgClosure_payload

-- rtsGetCharFunction = rtsGetIntFunction
-- generate a function which internally performs getInt, but is
-- named differently
generateRtsGetIntFunction ::
  BuiltinsOptions ->
  EntitySymbol -> -- Name of the function
  AsteriusModule
generateRtsGetIntFunction _ n = runEDSL n $ do
  setReturnTypes [I64]
  p <- param I64
  emit $ loadI64 (unTagClosure p) offset_StgClosure_payload

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
  p <- param I64
  emit $ loadI64 p 0

printI64Function :: BuiltinsOptions -> AsteriusModule
printI64Function _ = runEDSL "print_i64" $ do
  x <- param I64
  callImport "printI64" [convertSInt64ToFloat64 x]

assertEqI64Function :: BuiltinsOptions -> AsteriusModule
assertEqI64Function _ = runEDSL "assert_eq_i64" $ do
  x <- param I64
  y <- param I64
  callImport "assertEqI64" [convertSInt64ToFloat64 x, convertSInt64ToFloat64 y]

printF32Function :: BuiltinsOptions -> AsteriusModule
printF32Function _ = runEDSL "print_f32" $ do
  x <- param F32
  callImport "printF32" [x]

printF64Function :: BuiltinsOptions -> AsteriusModule
printF64Function _ = runEDSL "print_f64" $ do
  x <- param F64
  callImport "printF64" [x]

strlenFunction :: BuiltinsOptions -> AsteriusModule
strlenFunction _ = runEDSL "strlen" $ do
  setReturnTypes [I64]
  [str] <- params [I64]
  len <- callImport' "__asterius_strlen" [convertUInt64ToFloat64 str] F64
  emit $ truncUFloat64ToInt64 len

debugBelch2Function :: BuiltinsOptions -> AsteriusModule
debugBelch2Function _ = runEDSL "debugBelch2" $ do
  [fmt, str] <- params [I64, I64]
  callImport
    "__asterius_debugBelch2"
    [convertUInt64ToFloat64 fmt, convertUInt64ToFloat64 str]

memchrFunction :: BuiltinsOptions -> AsteriusModule
memchrFunction _ = runEDSL "memchr" $ do
  setReturnTypes [I64]
  [ptr, val, num] <- params [I64, I64, I64]
  p <-
    callImport'
      "__asterius_memchr"
      (map convertUInt64ToFloat64 [ptr, val, num])
      F64
  emit $ truncUFloat64ToInt64 p

threadPausedFunction :: BuiltinsOptions -> AsteriusModule
threadPausedFunction _ = runEDSL "threadPaused" $ do
  _ <- params [I64, I64]
  pure ()

dirtyMutVarFunction :: BuiltinsOptions -> AsteriusModule
dirtyMutVarFunction _ = runEDSL "dirty_MUT_VAR" $ do
  [_, p, _] <- params [I64, I64, I64]
  if'
    []
    (loadI64 p 0 `eqInt64` symbol "stg_MUT_VAR_CLEAN_info")
    (storeI64 p 0 $ symbol "stg_MUT_VAR_DIRTY_info")
    mempty

dirtyMVarFunction :: BuiltinsOptions -> AsteriusModule
dirtyMVarFunction _ = runEDSL "dirty_MVAR" $ do
  [_basereg, _mvar, _] <- params [I64, I64, I64]
  mempty

dirtyStackFunction :: BuiltinsOptions -> AsteriusModule
dirtyStackFunction _ = runEDSL "dirty_STACK" $ do
  [cap, stack] <- params [I64, I64]
  dirtySTACK cap stack

recordClosureMutatedFunction :: BuiltinsOptions -> AsteriusModule
recordClosureMutatedFunction _ = runEDSL "recordClosureMutated" $ do
  [_cap, _closure] <- params [I64, I64]
  mempty

tryWakeupThreadFunction :: BuiltinsOptions -> AsteriusModule
tryWakeupThreadFunction _ = runEDSL "tryWakeupThread" $ do
  [_cap, tso] <- params [I64, I64]
  callImport "__asterius_enqueueTSO" [wrapInt64 tso]

raiseExceptionHelperFunction :: BuiltinsOptions -> AsteriusModule
raiseExceptionHelperFunction _ = runEDSL "raiseExceptionHelper" $ do
  setReturnTypes [I64]
  args <- params [I64, I64, I64]
  frame_type <-
    truncUFloat64ToInt64
      <$> callImport'
        "__asterius_raiseExceptionHelper"
        (map convertUInt64ToFloat64 args)
        F64
  emit frame_type

-- Note that generateRTSWrapper will treat all our numbers as signed, not
-- unsigned.   This is OK for ASCII code, since the ints we have will not be
-- larger than 2^15.  However, for larger numbers, it would "overflow", and
-- would treat large unsigned  numbers as negative signed numbers.
unicodeCBits :: [(EntitySymbol, (FunctionImport, Function))]
unicodeCBits =
  map
    ( \(func_sym, param_vts, ret_vts) ->
        ( mkEntitySymbol func_sym,
          generateRTSWrapper "Unicode" func_sym param_vts ret_vts
        )
    )
    [ ("u_gencat", [I64], [I64]),
      ("u_iswalpha", [I64], [I64]),
      ("u_iswalnum", [I64], [I64]),
      ("u_iswupper", [I64], [I64]),
      ("u_iswlower", [I64], [I64]),
      ("u_towlower", [I64], [I64]),
      ("u_towupper", [I64], [I64]),
      ("u_towtitle", [I64], [I64]),
      ("u_iswcntrl", [I64], [I64]),
      ("u_iswprint", [I64], [I64])
    ]

suspendThreadFunction :: BuiltinsOptions -> AsteriusModule
suspendThreadFunction _ = runEDSL "suspendThread" $ do
  setReturnTypes [I64]
  [reg, _] <- params [I64, I64]
  emit reg

scheduleThreadFunction :: BuiltinsOptions -> AsteriusModule
scheduleThreadFunction _ = runEDSL "scheduleThread" $ do
  setReturnTypes []
  [_cap, tso] <- params [I64, I64]
  callImport "__asterius_enqueueTSO" [wrapInt64 tso]

scheduleThreadOnFunction :: BuiltinsOptions -> AsteriusModule
scheduleThreadOnFunction _ = runEDSL "scheduleThreadOn" $ do
  setReturnTypes []
  [_cap, _cpu, tso] <- params [I64, I64, I64]
  callImport "__asterius_enqueueTSO" [wrapInt64 tso]

resumeThreadFunction :: BuiltinsOptions -> AsteriusModule
resumeThreadFunction _ = runEDSL "resumeThread" $ do
  setReturnTypes [I64]
  reg <- param I64
  emit reg

performMajorGCFunction :: BuiltinsOptions -> AsteriusModule
performMajorGCFunction _ =
  runEDSL "performMajorGC" $ callImport "__asterius_performGC" []

performGCFunction :: BuiltinsOptions -> AsteriusModule
performGCFunction _ = runEDSL "performGC" $ call "performMajorGC" []

localeEncodingFunction :: BuiltinsOptions -> AsteriusModule
localeEncodingFunction _ = runEDSL "localeEncoding" $ do
  setReturnTypes [I64]
  emit $ symbol "__asterius_localeEncoding"

isattyFunction :: BuiltinsOptions -> AsteriusModule
isattyFunction _ = runEDSL "isatty" $ do
  setReturnTypes [I64]
  _ <- param I64
  emit $ constI64 0

fdReadyFunction :: BuiltinsOptions -> AsteriusModule
fdReadyFunction _ = runEDSL "fdReady" $ do
  setReturnTypes [I64]
  _ <- params [I64, I64, I64, I64]
  emit $ constI64 1

rtsSupportsBoundThreadsFunction :: BuiltinsOptions -> AsteriusModule
rtsSupportsBoundThreadsFunction _ = runEDSL "rtsSupportsBoundThreads" $ do
  setReturnTypes [I64]
  emit $ constI64 0

readFunction :: BuiltinsOptions -> AsteriusModule
readFunction _ =
  runEDSL "ghczuwrapperZC22ZCbaseZCSystemziPosixziInternalsZCread" $ do
    setReturnTypes [I64]
    [fd, buf, count] <- params [I64, I64, I64]
    r <-
      truncSFloat64ToInt64
        <$> callImport'
          "__asterius_read"
          (map convertUInt64ToFloat64 [fd, buf, count])
          F64
    emit r

writeFunction :: BuiltinsOptions -> AsteriusModule
writeFunction _ =
  runEDSL "ghczuwrapperZC20ZCbaseZCSystemziPosixziInternalsZCwrite" $ do
    setReturnTypes [I64]
    [fd, buf, count] <- params [I64, I64, I64]
    r <-
      truncSFloat64ToInt64
        <$> callImport'
          "__asterius_write"
          (map convertUInt64ToFloat64 [fd, buf, count])
          F64
    emit r

getF64GlobalRegFunction ::
  BuiltinsOptions ->
  -- Name of the function to be created
  EntitySymbol ->
  -- Global register to be returned
  UnresolvedGlobalReg ->
  AsteriusModule -- Module containing the function
getF64GlobalRegFunction _ n gr = runEDSL n $ do
  setReturnTypes [F64]
  emit $ convertSInt64ToFloat64 $ getLVal $ global gr

-- @cheng: there is a trade-off here: Either I emit the low-level
-- store and load, or I expose a _lot more_ from the EDSL
-- to create the correct types of stores and loads I want.
-- I went with the former, but we can discuss trade-offs.
-- Generate a wrap from the input type to the output type by invoking
-- the correct load instruction.
-- Since we only generate wrapping from larger types to smaller types,
-- our output can only be {32, 16, 8} bits. However, wasm has
-- I32 as the smallest type. So, our output is _always_ I32.
-- invariant: output type is smaller than input type.
genWrap ::
  -- Input type
  ValueType ->
  -- number of bytes to load for the output type
  Int ->
  Expression ->
  Expression
genWrap ti b x = Block
  { name = "",
    bodys =
      [ Store
          { bytes = if ti == I32 then 4 else 8,
            offset = 0,
            ptr = wrapInt64 (symbol "__asterius_i64_slot"),
            value = x,
            valueType = ti
          },
        Load
          { signed = False,
            bytes = fromIntegral b,
            offset = 0,
            valueType = I32,
            ptr = wrapInt64 (symbol "__asterius_i64_slot")
          }
      ],
    blockReturnTypes = [I32]
  }

-- Whether when generate a sign extended value
data ShouldSext
  = Sext
  | NoSext
  deriving (Eq)

-- generate a function to sign extend an input value into an output value.
-- We perform the sign extension by storing the old value.
-- Note that our input type is always I32. This is because we will only
-- ever have to generate sign extension calls from GHC.W8, GHC.W16, GHC.W32,
-- all of which are stored as I32, since wasm cannot store smaller types.
-- So, our input will _always_ be an I32.
genExtend ::
  -- number of bytes to load
  Int ->
  -- output value type
  ValueType ->
  -- whether the extend should sign-extend or not
  ShouldSext ->
  Expression ->
  Expression
genExtend b to sext x = Block
  { name = "",
    bodys =
      [ Store
          { bytes = 4,
            offset = 0,
            ptr = wrapInt64 (symbol "__asterius_i64_slot"),
            value = x,
            valueType = I32
          },
        Load
          { signed = sext == Sext,
            bytes = fromIntegral b,
            offset = 0,
            valueType = to,
            ptr = wrapInt64 (symbol "__asterius_i64_slot")
          }
      ],
    blockReturnTypes = [to]
  }
-- we will just use the i64 slot since it's large enough to hold all
-- the wasm datatypes we have.
