{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , defaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , rtsFunctionImports
  , rtsAsteriusFunctionExports
  , emitErrorMessage
  , wasmPageSize
  , generateWrapperFunction
  ) where

import Asterius.EDSL
import Asterius.Internals
import Asterius.Types
import qualified Data.ByteString.Short as SBS
import Data.Foldable
import Data.Functor
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)

wasmPageSize :: Int
wasmPageSize = 65536

data BuiltinsOptions = BuiltinsOptions
  { nurseryMBlocks, threadStateSize :: Int
  , tracing :: Bool
  }

defaultBuiltinsOptions :: BuiltinsOptions
defaultBuiltinsOptions =
  BuiltinsOptions
    {nurseryMBlocks = 512, threadStateSize = 65536, tracing = False}

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
        Map.fromList
          [ ( "MainCapability"
            , AsteriusStatics
                { staticsType = Bytes
                , asteriusStatics =
                    [ Serialized $
                      SBS.pack $
                      replicate (8 * roundup_bytes_to_words sizeof_Capability) 0
                    ]
                })
          ]
    , functionMap =
        Map.fromList $
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
        , ("scheduleWaitThread", scheduleWaitThreadFunction opts)
        , ("createThread", createThreadFunction opts)
        , ("createGenThread", createGenThreadFunction opts)
        , ("createIOThread", createIOThreadFunction opts)
        , ("createStrictIOThread", createStrictIOThreadFunction opts)
        , ("allocate", allocateFunction opts)
        , ("allocate_wrapper", allocateWrapperFunction opts)
        , ("allocateMightFail", allocateProxyFunction opts)
        , ("allocatePinned", allocateProxyFunction opts)
        , ("allocMegaGroup", allocMegaGroupFunction opts)
        , ("newCAF", newCAFFunction opts)
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
        , ("strlen", strlenFunction opts)
        , ("memchr", memchrFunction opts)
        , ("memcpy", memcpyFunction opts)
        , ("memset", memsetFunction opts)
        , ("memcmp", memcmpFunction opts)
        , ("__asterius_fromJSArrayBuffer", fromJSArrayBufferFunction opts)
        , ("__asterius_toJSArrayBuffer", toJSArrayBufferFunction opts)
        , ("__asterius_fromJSString", fromJSStringFunction opts)
        , ("__asterius_fromJSArray", fromJSArrayFunction opts)
        , ("threadPaused", threadPausedFunction opts)
        , ("dirty_MUT_VAR", dirtyMutVarFunction opts)
        ] <>
        map (\(func_sym, (_, func)) -> (func_sym, func)) byteStringCBits
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
      , externalModuleName = "StablePtr"
      , externalBaseName = "newStablePtr"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_deRefStablePtr"
      , externalModuleName = "StablePtr"
      , externalBaseName = "deRefStablePtr"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_freeStablePtr"
      , externalModuleName = "StablePtr"
      , externalBaseName = "freeStablePtr"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "printI64"
      , externalModuleName = "rts"
      , externalBaseName = "printI64"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = []}
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
      { internalName = "__asterius_eventI32"
      , externalModuleName = "rts"
      , externalBaseName = "emitEvent"
      , functionType = FunctionType {paramTypes = [I32], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_newTSO"
      , externalModuleName = "TSO"
      , externalBaseName = "newTSO"
      , functionType = FunctionType {paramTypes = [], returnTypes = [I32]}
      }
  , FunctionImport
      { internalName = "__asterius_setTSOret"
      , externalModuleName = "TSO"
      , externalBaseName = "setTSOret"
      , functionType = FunctionType {paramTypes = [I32, F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_setTSOrstat"
      , externalModuleName = "TSO"
      , externalBaseName = "setTSOrstat"
      , functionType = FunctionType {paramTypes = [I32, I32], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_getTSOret"
      , externalModuleName = "TSO"
      , externalBaseName = "getTSOret"
      , functionType = FunctionType {paramTypes = [I32], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_getTSOrstat"
      , externalModuleName = "TSO"
      , externalBaseName = "getTSOrstat"
      , functionType = FunctionType {paramTypes = [I32], returnTypes = [I32]}
      }
  , FunctionImport
      { internalName = "__asterius_allocMegaGroup"
      , externalModuleName = "MBlockAlloc"
      , externalBaseName = "allocMegaGroup"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_strlen"
      , externalModuleName = "Memory"
      , externalBaseName = "strlen"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_memchr"
      , externalModuleName = "Memory"
      , externalBaseName = "memchr"
      , functionType =
          FunctionType {paramTypes = [F64, F64, F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_memcpy"
      , externalModuleName = "Memory"
      , externalBaseName = "memcpy"
      , functionType =
          FunctionType {paramTypes = [F64, F64, F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_memmove"
      , externalModuleName = "Memory"
      , externalBaseName = "memmove"
      , functionType =
          FunctionType {paramTypes = [F64, F64, F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_memset"
      , externalModuleName = "Memory"
      , externalBaseName = "memset"
      , functionType =
          FunctionType {paramTypes = [F64, F64, F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_memcmp"
      , externalModuleName = "Memory"
      , externalBaseName = "memcmp"
      , functionType =
          FunctionType {paramTypes = [F64, F64, F64], returnTypes = [I32]}
      }
  , FunctionImport
      { internalName = "__asterius_fromJSArrayBuffer_imp"
      , externalModuleName = "Heap"
      , externalBaseName = "fromJSArrayBuffer"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_toJSArrayBuffer_imp"
      , externalModuleName = "Heap"
      , externalBaseName = "toJSArrayBuffer"
      , functionType =
          FunctionType {paramTypes = [F64, F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_fromJSString_imp"
      , externalModuleName = "Heap"
      , externalBaseName = "fromJSString"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_fromJSArray_imp"
      , externalModuleName = "Heap"
      , externalBaseName = "fromJSArray"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_checkRootTSO"
      , externalModuleName = "SanCheck"
      , externalBaseName = "checkRootTSO"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      }
  ] <>
  (if debug
     then [ FunctionImport
              { internalName = "__asterius_traceCmm"
              , externalModuleName = "Tracing"
              , externalBaseName = "traceCmm"
              , functionType =
                  FunctionType {paramTypes = [F64], returnTypes = []}
              }
          , FunctionImport
              { internalName = "__asterius_traceCmmBlock"
              , externalModuleName = "Tracing"
              , externalBaseName = "traceCmmBlock"
              , functionType =
                  FunctionType {paramTypes = [F64, I32], returnTypes = []}
              }
          , FunctionImport
              { internalName = "__asterius_traceCmmSetLocal"
              , externalModuleName = "Tracing"
              , externalBaseName = "traceCmmSetLocal"
              , functionType =
                  FunctionType {paramTypes = [F64, I32, F64], returnTypes = []}
              }
          , FunctionImport
              { internalName = "__asterius_loadI64"
              , externalModuleName = "MemoryTrap"
              , externalBaseName = "loadI64"
              , functionType =
                  FunctionType {paramTypes = [F64, I32, F64], returnTypes = []}
              }
          , FunctionImport
              { internalName = "__asterius_storeI64"
              , externalModuleName = "MemoryTrap"
              , externalBaseName = "storeI64"
              , functionType =
                  FunctionType {paramTypes = [F64, I32, F64], returnTypes = []}
              }
          ] <>
          concat
            [ [ FunctionImport
                  { internalName = "__asterius_load" <> k
                  , externalModuleName = "MemoryTrap"
                  , externalBaseName = "load" <> k
                  , functionType =
                      FunctionType
                        {paramTypes = [F64, I32, t], returnTypes = []}
                  }
              , FunctionImport
                  { internalName = "__asterius_store" <> k
                  , externalModuleName = "MemoryTrap"
                  , externalBaseName = "store" <> k
                  , functionType =
                      FunctionType
                        {paramTypes = [F64, I32, t], returnTypes = []}
                  }
            ]
            | (k, t) <-
                [ ("I8", I32)
                , ("I16", I32)
                , ("I32", I32)
                , ("F32", F32)
                , ("F64", F64)
                ]
            ]
     else []) <>
  map (fst . snd) byteStringCBits

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
  [ FunctionExport {internalName = "__asterius_" <> f, externalName = f}
  | f <- ["getTSOret", "getTSOrstat"]
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
  Block
    { name = ""
    , bodys = [EmitEvent {event = Event msg}, Unreachable]
    , blockReturnTypes = vts
    }

byteStringCBits :: [(AsteriusEntitySymbol, (FunctionImport, AsteriusFunction))]
byteStringCBits =
  map
    (\(func_sym, param_vts, ret_vts) ->
       ( AsteriusEntitySymbol func_sym
       , generateRTSWrapper "bytestring" func_sym param_vts ret_vts))
    [ ("fps_reverse", [I64, I64, I64], [])
    , ("fps_intersperse", [I64, I64, I64, I64], [])
    , ("fps_maximum", [I64, I64], [I64])
    , ("fps_minimum", [I64, I64], [I64])
    , ("fps_count", [I64, I64, I64], [I64])
    , ("fps_memcpy_offsets", [I64, I64, I64, I64, I64], [I64])
    , ("_hs_bytestring_int_dec", [I64, I64], [I64])
    , ("_hs_bytestring_long_long_int_dec", [I64, I64], [I64])
    , ("_hs_bytestring_uint_dec", [I64, I64], [I64])
    , ("_hs_bytestring_long_long_uint_dec", [I64, I64], [I64])
    , ("_hs_bytestring_int_dec_padded9", [I64, I64], [])
    , ("_hs_bytestring_long_long_int_dec_padded18", [I64, I64], [])
    , ("_hs_bytestring_uint_hex", [I64, I64], [I64])
    , ("_hs_bytestring_long_long_uint_hex", [I64, I64], [I64])
    ]

generateRTSWrapper ::
     SBS.ShortByteString
  -> SBS.ShortByteString
  -> [ValueType]
  -> [ValueType]
  -> (FunctionImport, AsteriusFunction)
generateRTSWrapper mod_sym func_sym param_vts ret_vts =
  ( FunctionImport
      { internalName = func_sym
      , externalModuleName = mod_sym
      , externalBaseName = func_sym
      , functionType =
          FunctionType {paramTypes = map fst xs, returnTypes = fst ret}
      }
  , AsteriusFunction
      { functionType =
          FunctionType {paramTypes = param_vts, returnTypes = ret_vts}
      , body =
          snd
            ret
            CallImport
              { target' = func_sym
              , operands = map snd xs
              , callImportReturnTypes = fst ret
              }
      })
  where
    xs =
      zipWith
        (\i vt ->
           case vt of
             I64 ->
               ( F64
               , convertSInt64ToFloat64 GetLocal {index = i, valueType = I64})
             _ -> (vt, GetLocal {index = i, valueType = vt}))
        [0 ..]
        param_vts
    ret =
      case ret_vts of
        [I64] -> ([F64], truncUFloat64ToInt64)
        _ -> (ret_vts, id)

generateWrapperFunction ::
     AsteriusEntitySymbol -> AsteriusFunction -> AsteriusFunction
generateWrapperFunction func_sym AsteriusFunction {functionType = FunctionType {..}} =
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
        I64 -> (i, F64, truncSFloat64ToInt64)
        _ -> (i, param_type, id)
      | (i, param_type) <- zip [0 ..] paramTypes
      ]
    (wrapper_return_types, to_wrapper_return_types) =
      case returnTypes of
        [I64] -> ([F64], convertSInt64ToFloat64)
        _ -> (returnTypes, id)

mainFunction, hsInitFunction, rtsApplyFunction, rtsEvalFunction, rtsEvalIOFunction, rtsEvalLazyIOFunction, rtsEvalStableIOFunction, rtsGetSchedStatusFunction, rtsCheckSchedStatusFunction, scheduleWaitThreadFunction, createThreadFunction, createGenThreadFunction, createIOThreadFunction, createStrictIOThreadFunction, allocateFunction, allocateWrapperFunction, allocateProxyFunction, allocMegaGroupFunction, newCAFFunction, stgReturnFunction, getStablePtrWrapperFunction, deRefStablePtrWrapperFunction, freeStablePtrWrapperFunction, rtsMkBoolFunction, rtsMkDoubleFunction, rtsMkCharFunction, rtsMkIntFunction, rtsMkWordFunction, rtsMkPtrFunction, rtsMkStablePtrFunction, rtsGetBoolFunction, rtsGetDoubleFunction, rtsGetCharFunction, rtsGetIntFunction, loadI64Function, printI64Function, printF32Function, printF64Function, strlenFunction, memchrFunction, memcpyFunction, memsetFunction, memcmpFunction, fromJSArrayBufferFunction, toJSArrayBufferFunction, fromJSStringFunction, fromJSArrayFunction, threadPausedFunction, dirtyMutVarFunction ::
     BuiltinsOptions -> AsteriusFunction
mainFunction BuiltinsOptions {..} =
  runEDSL [] $ do
    tid <- call' "rts_evalLazyIO" [symbol "Main_main_closure"] I32
    call "rts_checkSchedStatus" [tid]

initCapability :: EDSL ()
initCapability = do
  storeI32 mainCapability offset_Capability_no $ constI32 0
  storeI32 mainCapability offset_Capability_node $ constI32 0
  storeI8 mainCapability offset_Capability_in_haskell $ constI32 0
  storeI32 mainCapability offset_Capability_idle $ constI32 0
  storeI8 mainCapability offset_Capability_disabled $ constI32 0
  storeI64 mainCapability offset_Capability_total_allocated $ constI64 0
  storeI64
    mainCapability
    (offset_Capability_f + offset_StgFunTable_stgEagerBlackholeInfo) $
    symbol "__stg_EAGER_BLACKHOLE_info"
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

hsInitFunction _ =
  runEDSL [] $ do
    initCapability
    bd_nursery <- call' "allocMegaGroup" [constI64 1] I64
    block_nursery <- i64Local $ loadI64 bd_nursery offset_bdescr_start
    putLVal hp block_nursery
    putLVal hpLim $ block_nursery `addInt64` constI64 block_size
    putLVal hpAlloc (constI64 0)
    putLVal cccs (constI64 0)
    putLVal currentNursery bd_nursery
    bd_object_pool <- call' "allocMegaGroup" [constI64 1] I64
    storeI64 (getLVal baseReg) offset_StgRegTable_rCurrentAlloc bd_object_pool

rtsEvalHelper :: BuiltinsOptions -> AsteriusEntitySymbol -> EDSL ()
rtsEvalHelper BuiltinsOptions {..} create_thread_func_sym = do
  setReturnTypes [I32]
  p <- param I64
  tso <-
    call'
      create_thread_func_sym
      [mainCapability, constI64 $ roundup_bytes_to_words threadStateSize, p]
      I64
  call "scheduleWaitThread" [tso]
  emit $ loadI32 tso offset_StgTSO_id

rtsApplyFunction _ =
  runEDSL [I64] $ do
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

rtsEvalFunction opts = runEDSL [I32] $ rtsEvalHelper opts "createGenThread"

rtsEvalIOFunction opts =
  runEDSL [I32] $ rtsEvalHelper opts "createStrictIOThread"

rtsEvalLazyIOFunction opts = runEDSL [I32] $ rtsEvalHelper opts "createIOThread"

rtsEvalStableIOFunction BuiltinsOptions {..} =
  runEDSL [I32] $ do
    setReturnTypes [I32]
    s <- param I64
    p <- call' "deRefStablePtr" [s] I64
    tso <-
      call'
        "createStrictIOThread"
        [mainCapability, constI64 $ roundup_bytes_to_words threadStateSize, p]
        I64
    storeI32 tso offset_StgTSO_flags $
      loadI32 tso offset_StgTSO_flags `orInt32` constI32 tso_BLOCKEX `orInt32`
      constI32 tso_INTERRUPTIBLE
    call "scheduleWaitThread" [tso]
    ret_unwrapped <-
      truncUFloat64ToInt64 <$>
      callImport' "__asterius_getTSOret" [loadI32 tso offset_StgTSO_id] F64
    stat <- call' "rts_getSchedStatus" [loadI32 tso offset_StgTSO_id] I32
    if'
      []
      ((stat `eqInt32` constI32 scheduler_Success) `andInt32`
       (ret_unwrapped `neInt64` constI64 0))
      (call' "getStablePtr" [ret_unwrapped] I64 >>= \sptr ->
         callImport
           "__asterius_setTSOret"
           [loadI32 tso offset_StgTSO_id, convertUInt64ToFloat64 sptr])
      mempty
    emit $ loadI32 tso offset_StgTSO_id

rtsGetSchedStatusFunction _ =
  runEDSL [I32] $ do
    setReturnTypes [I32]
    tid <- param I32
    callImport' "__asterius_getTSOrstat" [tid] I32 >>= emit

rtsCheckSchedStatusFunction _ =
  runEDSL [] $ do
    tid <- param I32
    stat <- call' "rts_getSchedStatus" [tid] I32
    if' [] (stat `eqInt32` constI32 scheduler_Success) mempty $
      emit $
      emitErrorMessage
        []
        "rts_checkSchedStatus failed: illegal scheduler status code"

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

scheduleWaitThreadFunction BuiltinsOptions {..} =
  runEDSL [] $ do
    t <- param I64
    block' [] $ \sched_block_lbl ->
      loop' [] $ \sched_loop_lbl -> do
        if'
          []
          (loadI8 mainCapability offset_Capability_in_haskell)
          (emit (emitErrorMessage [] "Scheduler reentered from Haskell"))
          mempty
        storeI64
          mainCapability
          (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
          t
        storeI32 mainCapability offset_Capability_interrupt $ constI32 0
        storeI8 mainCapability offset_Capability_in_haskell $ constI32 1
        storeI32 mainCapability offset_Capability_idle $ constI32 0
        dirtyTSO mainCapability t
        dirtySTACK mainCapability (loadI64 t offset_StgTSO_stackobj)
        callImport "__asterius_checkRootTSO" [convertUInt64ToFloat64 t]
        r <- stgRun $ symbol "stg_returnToStackTop"
        ret <- i64Local $ loadI64 r offset_StgRegTable_rRet
        storeI8 mainCapability offset_Capability_in_haskell $ constI32 0
        switchI64 ret $
          const
            ( [ ( ret_HeapOverflow
                , do callImport
                       "__asterius_checkRootTSO"
                       [convertUInt64ToFloat64 t]
                     bytes <- i64Local $ getLVal hpAlloc
                     putLVal hpAlloc $ constI64 0
                     if'
                       []
                       (eqZInt64 bytes)
                       (emit $ emitErrorMessage [] "HeapOverflow with HpAlloc=0")
                       mempty
                     blocks <- i64Local $ bytesToMBlocks bytes
                     bd <- call' "allocMegaGroup" [blocks] I64
                     block <- i64Local $ loadI64 bd offset_bdescr_start
                     putLVal currentNursery bd
                     putLVal hp block
                     putLVal hpLim $
                       block `addInt64` (constI64 block_size `mulInt64` blocks)
                     break' sched_loop_lbl Nothing)
              , (ret_StackOverflow, emit $ emitErrorMessage [] "StackOverflow")
              , (ret_ThreadYielding, break' sched_loop_lbl Nothing)
              , (ret_ThreadBlocked, emit $ emitErrorMessage [] "ThreadBlocked")
              , ( ret_ThreadFinished
                , do callImport
                       "__asterius_checkRootTSO"
                       [convertUInt64ToFloat64 t]
                     if'
                       []
                       (loadI16 t offset_StgTSO_what_next `eqInt32`
                        constI32 next_ThreadComplete)
                       (do callImport
                             "__asterius_setTSOret"
                             [ loadI32 t offset_StgTSO_id
                             , convertUInt64ToFloat64 $
                               loadI64
                                 (loadI64
                                    (loadI64 t offset_StgTSO_stackobj)
                                    offset_StgStack_sp)
                                 8
                             ]
                           callImport
                             "__asterius_setTSOrstat"
                             [ loadI32 t offset_StgTSO_id
                             , constI32 scheduler_Success
                             ]
                           break' sched_block_lbl Nothing)
                       (do callImport
                             "__asterius_setTSOret"
                             [loadI32 t offset_StgTSO_id, ConstF64 0]
                           callImport
                             "__asterius_setTSOrstat"
                             [ loadI32 t offset_StgTSO_id
                             , constI32 scheduler_Killed
                             ]
                           break' sched_block_lbl Nothing))
              ]
            , emit $ emitErrorMessage [] "Illegal thread return code")

createThreadFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [cap, alloc_words] <- params [I64, I64]
    tso_p <- call' "allocate" [cap, alloc_words] I64
    stack_p <- i64Local $ tso_p `addInt64` constI64 offset_StgTSO_StgStack
    storeI64 stack_p 0 $ symbol "stg_STACK_info"
    stack_size_w <-
      i64Local $
      alloc_words `subInt64`
      constI64 ((offset_StgTSO_StgStack + offset_StgStack_stack) `div` 8)
    storeI32 stack_p offset_StgStack_stack_size $ wrapInt64 stack_size_w
    storeI64 stack_p offset_StgStack_sp $
      (stack_p `addInt64` constI64 offset_StgStack_stack) `addInt64`
      (stack_size_w `mulInt64` constI64 8)
    storeI32 stack_p offset_StgStack_dirty $ constI32 1
    storeI64 tso_p 0 $ symbol "stg_TSO_info"
    storeI16 tso_p offset_StgTSO_what_next $ constI32 next_ThreadRunGHC
    storeI16 tso_p offset_StgTSO_why_blocked $ constI32 blocked_NotBlocked
    storeI32 tso_p offset_StgTSO_flags $ constI32 0
    storeI32 tso_p offset_StgTSO_dirty $ constI32 1
    storeI32 tso_p offset_StgTSO_saved_errno $ constI32 0
    storeI64 tso_p offset_StgTSO_cap cap
    storeI64 tso_p offset_StgTSO_stackobj stack_p
    storeI32 tso_p offset_StgTSO_tot_stack_size $ wrapInt64 stack_size_w
    storeI64 tso_p offset_StgTSO_alloc_limit (constI64 0)
    storeI64 stack_p offset_StgStack_sp $
      loadI64 stack_p offset_StgStack_sp `subInt64`
      constI64 (8 * roundup_bytes_to_words sizeof_StgStopFrame)
    storeI64 (loadI64 stack_p offset_StgStack_sp) 0 $
      symbol "stg_stop_thread_info"
    callImport' "__asterius_newTSO" [] I32 >>= storeI32 tso_p offset_StgTSO_id
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

bytesToMBlocks :: Expression -> Expression
bytesToMBlocks bytes =
  (bytes `addInt64` constI64 (real_mblock_size - 1)) `divUInt64`
  constI64 real_mblock_size
  where
    real_mblock_size = mblock_size - offset_first_block

allocateFunction BuiltinsOptions {..} =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [_, n] <- params [I64, I64]
    bytes <- i64Local $ n `mulInt64` constI64 8
    if'
      [I64]
      (bytes `geUInt64` constI64 real_mblock_size)
      (do bd <- call' "allocMegaGroup" [bytesToMBlocks bytes] I64
          emit $ loadI64 bd offset_bdescr_free)
      (do if'
            []
            ((current_alloc_used `addInt64` bytes) `geUInt64`
             constI64 real_mblock_size)
            (call' "allocMegaGroup" [constI64 1] I64 >>=
             storeI64 (getLVal baseReg) offset_StgRegTable_rCurrentAlloc)
            mempty
          bd <- i64Local current_alloc
          free_prev <- i64Local $ loadI64 bd offset_bdescr_free
          storeI64 bd offset_bdescr_free $ free_prev `addInt64` bytes
          emit free_prev)
  where
    current_alloc = loadI64 (getLVal baseReg) offset_StgRegTable_rCurrentAlloc
    current_alloc_start = loadI64 current_alloc offset_bdescr_start
    current_alloc_free = loadI64 current_alloc offset_bdescr_free
    current_alloc_used = current_alloc_free `subInt64` current_alloc_start
    real_mblock_size = mblock_size - offset_first_block

allocateWrapperFunction _ =
  runEDSL [F64] $ do
    setReturnTypes [F64]
    n <- param F64
    r <-
      convertUInt64ToFloat64 <$>
      call' "allocate" [mainCapability, truncUFloat64ToInt64 n] I64
    emit r

allocateProxyFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [cap, n] <- params [I64, I64]
    call' "allocate" [cap, n] I64 >>= emit

allocMegaGroupFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    n <- param I64
    bd <-
      truncUFloat64ToInt64 <$>
      callImport' "__asterius_allocMegaGroup" [convertUInt64ToFloat64 n] F64
    emit bd

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

stgRun :: Expression -> EDSL Expression
stgRun init_f = do
  f <- i64MutLocal
  putLVal f init_f
  loop' [] $ \loop_lbl ->
    if' [] (eqZInt64 (getLVal f)) mempty $ do
      f' <-
        callIndirect'
          (getLVal f `subInt64` constI64 1)
          []
          (FunctionType [] [I64])
      putLVal f f'
      break' loop_lbl Nothing
  pure $ getLVal r1

stgReturnFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    emit $ constI64 0

getStablePtrWrapperFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    obj64 <- param I64
    sp_f64 <-
      callImport' "__asterius_newStablePtr" [convertUInt64ToFloat64 obj64] F64
    emit $ truncUFloat64ToInt64 sp_f64

deRefStablePtrWrapperFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    sp64 <- param I64
    obj_f64 <-
      callImport' "__asterius_deRefStablePtr" [convertUInt64ToFloat64 sp64] F64
    emit $ truncUFloat64ToInt64 obj_f64

freeStablePtrWrapperFunction _ =
  runEDSL [] $ do
    sp64 <- param I64
    callImport "__asterius_freeStablePtr" [convertUInt64ToFloat64 sp64]

rtsMkHelper :: BuiltinsOptions -> AsteriusEntitySymbol -> AsteriusFunction
rtsMkHelper _ con_sym =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [i] <- params [I64]
    p <- call' "allocate" [mainCapability, constI64 2] I64
    storeI64 p 0 $ symbol con_sym
    storeI64 p 8 i
    emit p

rtsMkBoolFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [i] <- params [I64]
    if'
      [I64]
      (eqZInt64 i)
      (emit $ symbol' "ghczmprim_GHCziTypes_False_closure" 1)
      (emit $ symbol' "ghczmprim_GHCziTypes_True_closure" 2)

rtsMkDoubleFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [i] <- params [F64]
    p <- call' "allocate" [mainCapability, constI64 2] I64
    storeI64 p 0 $ symbol "ghczmprim_GHCziTypes_Dzh_con_info"
    storeF64 p 8 i
    emit p

rtsMkCharFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [i] <- params [I64]
    p <- call' "allocate" [mainCapability, constI64 2] I64
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
    callImport "printI64" [convertSInt64ToFloat64 x]

printF32Function _ =
  runEDSL [] $ do
    x <- param F32
    callImport "printF32" [x]

printF64Function _ =
  runEDSL [] $ do
    x <- param F64
    callImport "printF64" [x]

strlenFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [str] <- params [I64]
    len <- callImport' "__asterius_strlen" [convertUInt64ToFloat64 str] F64
    emit $ truncUFloat64ToInt64 len

memchrFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [ptr, val, num] <- params [I64, I64, I64]
    p <-
      callImport'
        "__asterius_memchr"
        (map (convertUInt64ToFloat64) [ptr, val, num])
        F64
    emit $ truncUFloat64ToInt64 p

memcpyFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [dst, src, n] <- params [I64, I64, I64]
    callImport "__asterius_memcpy" $ map (convertUInt64ToFloat64) [dst, src, n]
    emit dst

memsetFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [dst, c, n] <- params [I64, I64, I64]
    callImport "__asterius_memset" $ map (convertUInt64ToFloat64) [dst, c, n]
    emit dst

memcmpFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [ptr1, ptr2, n] <- params [I64, I64, I64]
    cres <-
      callImport'
        "__asterius_memcmp"
        (map (convertUInt64ToFloat64) [ptr1, ptr2, n])
        I32
    emit $ Unary ExtendSInt32 cres

fromJSArrayBufferFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [buf] <- params [I64]
    addr <-
      truncUFloat64ToInt64 <$>
      callImport'
        "__asterius_fromJSArrayBuffer_imp"
        [convertUInt64ToFloat64 buf]
        F64
    emit addr

toJSArrayBufferFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [addr, len] <- params [I64, I64]
    r <-
      truncUFloat64ToInt64 <$>
      callImport'
        "__asterius_toJSArrayBuffer_imp"
        (map (convertUInt64ToFloat64) [addr, len])
        F64
    emit r

fromJSStringFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [s] <- params [I64]
    addr <-
      truncUFloat64ToInt64 <$>
      callImport' "__asterius_fromJSString_imp" [convertUInt64ToFloat64 s] F64
    emit addr

fromJSArrayFunction _ =
  runEDSL [I64] $ do
    setReturnTypes [I64]
    [arr] <- params [I64]
    addr <-
      truncUFloat64ToInt64 <$>
      callImport' "__asterius_fromJSArray_imp" [convertUInt64ToFloat64 arr] F64
    emit addr

threadPausedFunction _ = runEDSL [] $ void $ params [I64, I64]

dirtyMutVarFunction _ =
  runEDSL [] $ do
    [_, p] <- params [I64, I64]
    if'
      []
      (loadI64 p 0 `eqInt64` symbol "stg_MUT_VAR_CLEAN_info")
      (storeI64 p 0 $ symbol "stg_MUT_VAR_DIRTY_info")
      mempty

getF64GlobalRegFunction ::
     BuiltinsOptions -> UnresolvedGlobalReg -> AsteriusFunction
getF64GlobalRegFunction _ gr =
  runEDSL [F64] $ do
    setReturnTypes [F64]
    emit $ convertSInt64ToFloat64 $ getLVal $ global gr

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
                        (I32, 1) -> "__asterius_loadI8"
                        (I32, 2) -> "__asterius_loadI16"
                        (I32, 4) -> "__asterius_loadI32"
                        (I64, 8) -> "__asterius_loadI64"
                        (F32, 4) -> "__asterius_loadF32"
                        (F64, 8) -> "__asterius_loadF64"
                        _ ->
                          error $
                          "Unsupported ValueType/ByteLength: " <> show (vt, b)
                  , operands =
                      [ convertSInt64ToFloat64 p
                      , o
                      , case vt of
                          I64 -> convertSInt64ToFloat64 v
                          _ -> v
                      ]
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
                        (I32, 1) -> "__asterius_storeI8"
                        (I32, 2) -> "__asterius_storeI16"
                        (I32, 4) -> "__asterius_storeI32"
                        (I64, 8) -> "__asterius_storeI64"
                        (F32, 4) -> "__asterius_storeF32"
                        (F64, 8) -> "__asterius_storeF64"
                        _ ->
                          error $
                          "Unsupported ValueType/ByteLength: " <> show (vt, b)
                  , operands =
                      [ convertSInt64ToFloat64 p
                      , o
                      , case vt of
                          I64 -> convertSInt64ToFloat64 v
                          _ -> v
                      ]
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
