{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , defaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , rtsFunctionImports
  , rtsFunctionExports
  , emitErrorMessage
  , wasmPageSize
  , generateWrapperFunction
  , ShouldSext(..)
  , genWrap
  , genExtend
  ) where

import Asterius.EDSL
import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Data.ByteString.Short as SBS
import Data.Foldable
import Data.Functor
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String
import Data.Word
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)

wasmPageSize :: Int
wasmPageSize = 65536

data BuiltinsOptions = BuiltinsOptions
  { progName :: String
  , threadStateSize :: Int
  , debug, hasMain :: Bool
  } deriving (Show)

defaultBuiltinsOptions :: BuiltinsOptions
defaultBuiltinsOptions =
  BuiltinsOptions
    { progName =
        error "Asterius.Builtins.defaultBuiltinsOptions: unknown progName"
    , threadStateSize = 65536
    , debug = False
    , hasMain = True
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
          , ( "rts_stop_on_exception"
            , AsteriusStatics
                { staticsType = Bytes
                , asteriusStatics = [Serialized $ encodeStorable (0 :: Word64)]
                })
          , ( "n_capabilities"
            , AsteriusStatics
                { staticsType = ConstBytes
                , asteriusStatics = [Serialized $ encodeStorable (1 :: Word32)]
                })
          , ( "enabled_capabilities"
            , AsteriusStatics
                { staticsType = ConstBytes
                , asteriusStatics = [Serialized $ encodeStorable (1 :: Word32)]
                })
          , ( "prog_name"
            , AsteriusStatics
                { staticsType = ConstBytes
                , asteriusStatics =
                    [Serialized $ fromString (progName opts <> "\0")]
                })
          , ( "prog_argv"
            , AsteriusStatics
                { staticsType = ConstBytes
                , asteriusStatics = [SymbolStatic "prog_name" 0]
                })
          , ( "__asterius_localeEncoding"
            , AsteriusStatics
                { staticsType = ConstBytes
                , asteriusStatics = [Serialized "UTF-8\0"]
                })
          , ( "__asterius_pc"
            , AsteriusStatics
                { staticsType = Bytes
                , asteriusStatics = [Serialized $ encodeStorable invalidAddress]
                })
          , ( "__asterius_i64_slot"
            , AsteriusStatics
                { staticsType = Bytes
                , asteriusStatics = [Serialized $ SBS.pack $ replicate 8 0]
                })
          ]
    , functionMap =
        Map.fromList $
        map (\(func_sym, (_, func)) -> (func_sym, func))
            (byteStringCBits <> floatCBits  <> unicodeCBits <> md5CBits)
    }  <> hsInitFunction opts
       <> createThreadFunction opts
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
       <> memcpyFunction opts
       <> memsetFunction opts
       <> memcmpFunction opts
       <> fromJSArrayBufferFunction opts
       <> toJSArrayBufferFunction opts
       <> fromJSStringFunction opts
       <> fromJSArrayFunction opts
       <> threadPausedFunction opts
       <> dirtyMutVarFunction opts
       <> raiseExceptionHelperFunction opts
       <> barfFunction opts
       <> getProgArgvFunction opts
       <> suspendThreadFunction opts
       <> resumeThreadFunction opts
       <> performMajorGCFunction opts
       <> performGCFunction opts
       <> localeEncodingFunction opts
       <> (if debug opts then generateRtsAsteriusDebugModule opts else mempty)
       -- Add in the module that contain functions which need to be
       -- exposed to the outside world. So add in the module, and
       -- the module wrapped by using `generateWrapperModule`.
       <> generateRtsExternalInterfaceModule opts
       <> generateWrapperModule (generateRtsExternalInterfaceModule opts)


-- | Generate the module consisting of functions which need to be wrapped
-- | for communication with the external runtime.
generateRtsExternalInterfaceModule :: BuiltinsOptions -> AsteriusModule
generateRtsExternalInterfaceModule opts = mempty
  <> rtsApplyFunction opts
  <> createGenThreadFunction opts
  <> createIOThreadFunction opts
  <> createStrictIOThreadFunction opts
  <> scheduleWaitThreadFunction opts
  <> rtsGetSchedStatusFunction opts
  <> rtsCheckSchedStatusFunction opts
  <> getStablePtrWrapperFunction opts
  <> deRefStablePtrWrapperFunction opts
  <> freeStablePtrWrapperFunction opts
  <> rtsMkBoolFunction opts
  <> rtsMkDoubleFunction opts
  <> rtsMkCharFunction opts
  <> rtsMkIntFunction opts
  <> rtsMkWordFunction opts
  <> rtsMkPtrFunction opts
  <> rtsMkStablePtrFunction opts
  <> rtsGetBoolFunction opts
  <> rtsGetDoubleFunction opts
  <> generateRtsGetIntFunction opts "rts_getChar"
  <> generateRtsGetIntFunction opts "rts_getInt"
  <> generateRtsGetIntFunction opts "rts_getWord"
  <> generateRtsGetIntFunction opts "rts_getPtr"
  <> generateRtsGetIntFunction opts "rts_getStablePtr"
  <> loadI64Function opts

-- | Generate the module consisting of debug functions
generateRtsAsteriusDebugModule :: BuiltinsOptions -> AsteriusModule
generateRtsAsteriusDebugModule opts = mempty
  <> getF64GlobalRegFunction opts  "__asterius_Load_Sp" Sp
  <> getF64GlobalRegFunction opts  "__asterius_Load_SpLim" SpLim
  <> getF64GlobalRegFunction opts  "__asterius_Load_Hp" Hp
  <> getF64GlobalRegFunction opts  "__asterius_Load_HpLim" SpLim

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
      , "asinh"
      , "acosh"
      , "atanh"
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
      { internalName = "assertEqI64"
      , externalModuleName = "rts"
      , externalBaseName = "assertEqI64"
      , functionType = FunctionType {paramTypes = [F64, F64], returnTypes = []}
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
      { internalName = "__asterius_hpAlloc"
      , externalModuleName = "HeapAlloc"
      , externalBaseName = "hpAlloc"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_allocate"
      , externalModuleName = "HeapAlloc"
      , externalBaseName = "allocate"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_allocatePinned"
      , externalModuleName = "HeapAlloc"
      , externalBaseName = "allocatePinned"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_strlen"
      , externalModuleName = "Memory"
      , externalBaseName = "strlen"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_debugBelch2"
      , externalModuleName = "Messages"
      , externalBaseName = "debugBelch2"
      , functionType = FunctionType {paramTypes = [F64, F64], returnTypes = []}
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
      , externalModuleName = "HeapBuilder"
      , externalBaseName = "fromJSArrayBuffer"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_toJSArrayBuffer_imp"
      , externalModuleName = "HeapBuilder"
      , externalBaseName = "toJSArrayBuffer"
      , functionType =
          FunctionType {paramTypes = [F64, F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_fromJSString_imp"
      , externalModuleName = "HeapBuilder"
      , externalBaseName = "fromJSString"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_fromJSArray_imp"
      , externalModuleName = "HeapBuilder"
      , externalBaseName = "fromJSArray"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_gcRootTSO"
      , externalModuleName = "GC"
      , externalBaseName = "gcRootTSO"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_raiseExceptionHelper"
      , externalModuleName = "ExceptionHelper"
      , externalBaseName = "raiseExceptionHelper"
      , functionType =
          FunctionType {paramTypes = [F64, F64, F64], returnTypes = [F64]}
      }
  , FunctionImport
      { internalName = "__asterius_barf"
      , externalModuleName = "ExceptionHelper"
      , externalBaseName = "barf"
      , functionType = FunctionType {paramTypes = [F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_threadPaused"
      , externalModuleName = "ThreadPaused"
      , externalBaseName = "threadPaused"
      , functionType = FunctionType {paramTypes = [F64, F64], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_enter"
      , externalModuleName = "ReentrancyGuard"
      , externalBaseName = "enter"
      , functionType = FunctionType {paramTypes = [I32], returnTypes = []}
      }
  , FunctionImport
      { internalName = "__asterius_exit"
      , externalModuleName = "ReentrancyGuard"
      , externalBaseName = "exit"
      , functionType = FunctionType {paramTypes = [I32], returnTypes = []}
      }

  , FunctionImport
      { internalName = "__asterius_mul2"
      , externalModuleName = "Integer"
      , externalBaseName = "mul2"
      , functionType = FunctionType {
          paramTypes = [I32, I32, I32, I32, I32]
          , returnTypes = [I32]
          }
      }
  , FunctionImport
      { internalName = "__asterius_quotrem2_quotient"
      , externalModuleName = "Integer"
      , externalBaseName = "quotrem2_quotient"
      , functionType = FunctionType {
          paramTypes = [I32, I32, I32, I32, I32, I32, I32]
          , returnTypes = [I32]
          }
      }
  , FunctionImport
      { internalName = "__asterius_quotrem2_remainder"
      , externalModuleName = "Integer"
      , externalBaseName = "quotrem2_remainder"
      , functionType = FunctionType {
          paramTypes = [I32, I32, I32, I32, I32, I32, I32]
          , returnTypes = [I32]
          }
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
          ] <>
          concat
            [ [ FunctionImport
                  { internalName = "__asterius_load_" <> k
                  , externalModuleName = "MemoryTrap"
                  , externalBaseName = "load" <> k
                  , functionType =
                      FunctionType {paramTypes = [I64, I32], returnTypes = [t]}
                  }
              , FunctionImport
                  { internalName = "__asterius_store_" <> k
                  , externalModuleName = "MemoryTrap"
                  , externalBaseName = "store" <> k
                  , functionType =
                      FunctionType
                        {paramTypes = [I64, I32, t], returnTypes = []}
                  }
            ]
            | (k, t) <-
                [ ("I8", I32)
                , ("I16", I32)
                , ("I32", I32)
                , ("I64", I64)
                , ("F32", F32)
                , ("F64", F64)
                ]
            ] <>
          [ FunctionImport
            { internalName = "__asterius_load_" <> k1 <> "_" <> s <> b
            , externalModuleName = "MemoryTrap"
            , externalBaseName = "load" <> k1 <> s <> b
            , functionType =
                FunctionType {paramTypes = [I64, I32], returnTypes = [t1]}
            }
          | (k1, t1) <- [("I32", I32), ("I64", I64)]
          , s <- ["S", "U"]
          , b <- ["8", "16"]
          ]
     else []) <>
  map (fst . snd) (byteStringCBits <> floatCBits <> unicodeCBits <> md5CBits)

rtsFunctionExports :: Bool -> [FunctionExport]
rtsFunctionExports debug =
  [ FunctionExport {internalName = f <> "_wrapper", externalName = f}
  | f <-
      [ "loadI64"
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
      , "createGenThread"
      , "createStrictIOThread"
      , "createIOThread"
      , "scheduleWaitThread"
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
      ["hs_init"]
  ]

emitErrorMessage :: [ValueType] -> SBS.ShortByteString -> Expression
emitErrorMessage vts ev = Barf {barfMessage = ev, barfReturnTypes = vts}

byteStringCBits :: [(AsteriusEntitySymbol, (FunctionImport, Function))]
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

floatCBits :: [(AsteriusEntitySymbol, (FunctionImport, Function))]
floatCBits =
    map (\(func_sym, param_vts, ret_vts) ->
       ( AsteriusEntitySymbol func_sym
       , generateRTSWrapper "floatCBits" func_sym param_vts ret_vts))
    [ ("isFloatNegativeZero", [F32], [I64])
    , ("isDoubleNegativeZero", [F64], [I64])
    , ("isFloatNaN", [F32], [I64])
    , ("isDoubleNaN", [F64], [I64])
    , ("isFloatFinite", [F32], [I64])
    , ("isDoubleFinite", [F64], [I64])
    , ("isFloatDenormalized", [F32], [I64])
    , ("isDoubleDenormalized", [F64], [I64])
    , ("isFloatInfinite", [F32], [I64])
    , ("isDoubleInfinite", [F64], [I64])
    , ("__decodeFloat_Int", [I64, I64, F32], [])
    , ("rintDouble", [F64], [F64])
    , ("rintFloat", [F32], [F32])
    ]

md5CBits :: [(AsteriusEntitySymbol, (FunctionImport, Function))]
md5CBits =
    map (\(func_sym, param_vts, ret_vts) ->
       ( AsteriusEntitySymbol func_sym
       , generateRTSWrapper "MD5" func_sym param_vts ret_vts))
    [ ("__hsbase_MD5Init", [I64], [])
    , ("__hsbase_MD5Update", [I64, I64, I64], [])
    , ("__hsbase_MD5Final", [I64, I64], [])
    ]

generateRTSWrapper ::
     SBS.ShortByteString
  -> SBS.ShortByteString
  -> [ValueType]
  -> [ValueType]
  -> (FunctionImport, Function)
generateRTSWrapper mod_sym func_sym param_vts ret_vts =
  ( FunctionImport
      { internalName = "__asterius_" <> func_sym
      , externalModuleName = mod_sym
      , externalBaseName = func_sym
      , functionType =
          FunctionType {paramTypes = map fst xs, returnTypes = fst ret}
      }
  , Function
      { functionType =
          FunctionType {paramTypes = param_vts, returnTypes = ret_vts}
      , varTypes = []
      , body =
          snd
            ret
            CallImport
              { target' = "__asterius_" <> func_sym
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

generateWrapperFunction :: AsteriusEntitySymbol -> Function -> Function
generateWrapperFunction func_sym Function {functionType = FunctionType {..}} =
  Function
    { functionType =
        FunctionType
          { paramTypes =
              [ wrapper_param_type
              | (_, wrapper_param_type, _) <- wrapper_param_types
              ]
          , returnTypes = wrapper_return_types
          }
    , varTypes = []
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


-- | Renames each function in the module to <name>_wrapper, and
-- | edits their implementation using 'generateWrapperFunction'
generateWrapperModule :: AsteriusModule -> AsteriusModule
generateWrapperModule mod = mod {
	functionMap=Map.fromList $ map (\(n, f) -> (n <> "_wrapper", generateWrapperFunction n f)) (Map.toList $ functionMap mod)
}




hsInitFunction, rtsApplyFunction, rtsGetSchedStatusFunction, rtsCheckSchedStatusFunction, scheduleWaitThreadFunction, createThreadFunction, createGenThreadFunction, createIOThreadFunction, createStrictIOThreadFunction, allocatePinnedFunction, newCAFFunction, stgReturnFunction, getStablePtrWrapperFunction, deRefStablePtrWrapperFunction, freeStablePtrWrapperFunction, rtsMkBoolFunction, rtsMkDoubleFunction, rtsMkCharFunction, rtsMkIntFunction, rtsMkWordFunction, rtsMkPtrFunction, rtsMkStablePtrFunction, rtsGetBoolFunction, rtsGetDoubleFunction, loadI64Function, printI64Function, assertEqI64Function, printF32Function, printF64Function, strlenFunction, memchrFunction, memcpyFunction, memsetFunction, memcmpFunction, fromJSArrayBufferFunction, toJSArrayBufferFunction, fromJSStringFunction, fromJSArrayFunction, threadPausedFunction, dirtyMutVarFunction, raiseExceptionHelperFunction, barfFunction, getProgArgvFunction, suspendThreadFunction, resumeThreadFunction, performMajorGCFunction, performGCFunction, localeEncodingFunction ::
     BuiltinsOptions -> AsteriusModule

initCapability :: EDSL ()
initCapability = do
  storeI32 mainCapability offset_Capability_no $ constI32 0
  storeI32 mainCapability offset_Capability_node $ constI32 0
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
  runEDSL "hs_init" $ do
    initCapability
    bd_nursery <-
      truncUFloat64ToInt64 <$> callImport' "__asterius_hpAlloc" [constF64 8] F64
    putLVal currentNursery bd_nursery

enter, exit :: Int -> EDSL ()
enter i = callImport "__asterius_enter" [constI32 i]

exit i = callImport "__asterius_exit" [constI32 i]

rtsApplyFunction _ =
  runEDSL "rts_apply" $ do
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

rtsGetSchedStatusFunction _ =
  runEDSL "rts_getSchedStatus" $ do
    setReturnTypes [I32]
    tid <- param I32
    callImport' "__asterius_getTSOrstat" [tid] I32 >>= emit

rtsCheckSchedStatusFunction _ =
  runEDSL"rts_checkSchedStatus" $ do
    tid <- param I32
    stat <- call' "rts_getSchedStatus" [tid] I32
    if' [] (stat `eqInt32` constI32 scheduler_Success) mempty $
      emit $ emitErrorMessage [] "IllegalSchedulerStatusCode"

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

scheduleWaitThreadFunction BuiltinsOptions {} =
  runEDSL "scheduleWaitThread" $ do
    [t, load_regs] <- params [I64, I32]
    tid <- i32Local $ loadI32 t offset_StgTSO_id
    block' [] $ \sched_block_lbl ->
      loop' [] $ \sched_loop_lbl -> do
        storeI64
          mainCapability
          (offset_Capability_r + offset_StgRegTable_rCurrentTSO)
          t
        storeI32 mainCapability offset_Capability_interrupt $ constI32 0
        storeI32 mainCapability offset_Capability_idle $ constI32 0
        dirtyTSO mainCapability t
        dirtySTACK mainCapability (loadI64 t offset_StgTSO_stackobj)
        r <- stgRun $ symbol "stg_returnToStackTop"
        ret <- i64Local $ loadI64 r offset_StgRegTable_rRet
        switchI64 ret $
          const
            ( [ ( ret_HeapOverflow
                , do callImport
                       "__asterius_gcRootTSO"
                       [convertUInt64ToFloat64 t]
                     bytes <- i64Local $ getLVal hpAlloc
                     putLVal hpAlloc $ constI64 0
                     if'
                       []
                       (eqZInt64 bytes)
                       (emit $ emitErrorMessage [] "HeapOverflowWithZeroHpAlloc")
                       mempty
                     truncUFloat64ToInt64 <$>
                       callImport'
                         "__asterius_hpAlloc"
                         [convertUInt64ToFloat64 bytes]
                         F64 >>=
                       putLVal currentNursery
                     break' sched_loop_lbl Nothing)
              , (ret_StackOverflow, emit $ emitErrorMessage [] "StackOverflow")
              , (ret_ThreadYielding, break' sched_loop_lbl Nothing)
              , (ret_ThreadBlocked, emit $ emitErrorMessage [] "ThreadBlocked")
              , ( ret_ThreadFinished
                , if'
                    []
                    (loadI16 t offset_StgTSO_what_next `eqInt32`
                     constI32 next_ThreadComplete)
                    (do callImport
                          "__asterius_setTSOret"
                          [ tid
                          , convertUInt64ToFloat64 $
                            loadI64
                              (loadI64
                                 (loadI64 t offset_StgTSO_stackobj)
                                 offset_StgStack_sp)
                              8
                          ]
                        callImport
                          "__asterius_setTSOrstat"
                          [tid, constI32 scheduler_Success]
                        break' sched_block_lbl Nothing)
                    (do callImport "__asterius_setTSOret" [tid, ConstF64 0]
                        callImport
                          "__asterius_setTSOrstat"
                          [tid, constI32 scheduler_Killed]
                        break' sched_block_lbl Nothing))
              ]
            , emit $ emitErrorMessage [] "IllegalThreadReturnCode")

createThreadFunction BuiltinsOptions {..} =
  runEDSL "createThread" $ do
    setReturnTypes [I64]
    let alloc_words = constI64 $ roundup_bytes_to_words threadStateSize
    tso_p <- call' "allocatePinned" [mainCapability, alloc_words] I64
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
  closure <- param I64
  t <- call' "createThread" [] I64
  for_ (mk_closures closure) $ pushClosure t
  emit t

createGenThreadFunction _ =
  runEDSL "createGenThread" $
  createThreadHelper $ \closure -> [closure, symbol "stg_enter_info"]

createIOThreadFunction _ =
  runEDSL "createIOThread" $
  createThreadHelper $ \closure ->
    [symbol "stg_ap_v_info", closure, symbol "stg_enter_info"]

createStrictIOThreadFunction _ =
  runEDSL "createStrictIOThread"  $
  createThreadHelper $ \closure ->
    [ symbol "stg_forceIO_info"
    , symbol "stg_ap_v_info"
    , closure
    , symbol "stg_enter_info"
    ]


genAllocateFunction :: BuiltinsOptions
    -> AsteriusEntitySymbol -- ^ Name of the allocation function
    ->  AsteriusModule -- ^ Module representing the function
genAllocateFunction (BuiltinsOptions {}) n =
  runEDSL n $ do
    setReturnTypes [I64]
    [_, n] <- params [I64, I64]
    (truncUFloat64ToInt64 <$>
     callImport' "__asterius_allocate" [convertUInt64ToFloat64 n] F64) >>=
      emit

{-
allocateFunction BuiltinsOptions {} =
  runEDSL "allocate" $ do
    setReturnTypes [I64]
    [_, n] <- params [I64, I64]
    (truncUFloat64ToInt64 <$>
     callImport' "__asterius_allocate" [convertUInt64ToFloat64 n] F64) >>=
      emit
  -}

allocatePinnedFunction _ =
  runEDSL "allocatePinned" $ do
    setReturnTypes [I64]
    [_, n] <- params [I64, I64]
    (truncUFloat64ToInt64 <$>
     callImport' "__asterius_allocatePinned" [convertUInt64ToFloat64 n] F64) >>=
      emit

newCAFFunction _ =
  runEDSL "newCAF" $ do
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
  let pc = pointerI64 (symbol "__asterius_pc") 0
  pc_reg <- i64MutLocal
  putLVal pc init_f
  loop' [] $ \loop_lbl -> do
    putLVal pc_reg $ getLVal pc
    if' [] (eqZInt64 (getLVal pc_reg)) mempty $ do
      callIndirect (getLVal pc_reg)
      break' loop_lbl Nothing
  pure $ getLVal r1

stgReturnFunction _ =
  runEDSL "StgReturn" $ storeI64 (symbol "__asterius_pc") 0 $ constI64 0

getStablePtrWrapperFunction _ =
  runEDSL "getStablePtr"  $ do
    setReturnTypes [I64]
    obj64 <- param I64
    sp_f64 <-
      callImport' "__asterius_newStablePtr" [convertUInt64ToFloat64 obj64] F64
    emit $ truncUFloat64ToInt64 sp_f64

deRefStablePtrWrapperFunction _ =
  runEDSL "deRefStablePtr" $ do
    setReturnTypes [I64]
    sp64 <- param I64
    obj_f64 <-
      callImport' "__asterius_deRefStablePtr" [convertUInt64ToFloat64 sp64] F64
    emit $ truncUFloat64ToInt64 obj_f64

freeStablePtrWrapperFunction _ =
  runEDSL"hs_free_stable_ptr"  $ do
    sp64 <- param I64
    callImport "__asterius_freeStablePtr" [convertUInt64ToFloat64 sp64]

rtsMkHelper ::
   BuiltinsOptions
  -> AsteriusEntitySymbol -- ^ Name of the function to be built
  -> AsteriusEntitySymbol -- ^ Mangled name of the primop constructor
  -> AsteriusModule
rtsMkHelper _ n con_sym =
  runEDSL n  $ do
    setReturnTypes [I64]
    [i] <- params [I64]
    p <- call' "allocate" [mainCapability, constI64 2] I64
    storeI64 p 0 $ symbol con_sym
    storeI64 p 8 i
    emit p

rtsMkBoolFunction _ =
  runEDSL "rts_mkBool" $ do
    setReturnTypes [I64]
    [i] <- params [I64]
    if'
      [I64]
      (eqZInt64 i)
      (emit $ symbol' "ghczmprim_GHCziTypes_False_closure" 1)
      (emit $ symbol' "ghczmprim_GHCziTypes_True_closure" 2)

rtsMkDoubleFunction _ =
  runEDSL "rts_mkDouble" $ do
    setReturnTypes [I64]
    [i] <- params [F64]
    p <- call' "allocate" [mainCapability, constI64 2] I64
    storeI64 p 0 $ symbol "ghczmprim_GHCziTypes_Dzh_con_info"
    storeF64 p 8 i
    emit p

rtsMkCharFunction _ =
  runEDSL "rts_mkChar" $ do
    setReturnTypes [I64]
    [i] <- params [I64]
    p <- call' "allocate" [mainCapability, constI64 2] I64
    storeI64 p 0 $ symbol "ghczmprim_GHCziTypes_Czh_con_info"
    storeI64 p 8 i
    emit p

rtsMkIntFunction opts = rtsMkHelper opts "rts_mkInt" "ghczmprim_GHCziTypes_Izh_con_info"

rtsMkWordFunction opts = rtsMkHelper opts "rts_mkWord" "ghczmprim_GHCziTypes_Wzh_con_info"

rtsMkPtrFunction opts = rtsMkHelper opts "rts_mkPtr" "base_GHCziPtr_Ptr_con_info"

rtsMkStablePtrFunction opts =
  rtsMkHelper opts "rts_mkStablePtr" "base_GHCziStable_StablePtr_con_info"

unTagClosure :: Expression -> Expression
unTagClosure p = p `andInt64` constI64 0xFFFFFFFFFFFFFFF8

rtsGetBoolFunction _ =
  runEDSL "rts_getBool" $ do
    setReturnTypes [I64]
    p <- param I64
    emit $
      extendUInt32 $
      neInt32
        (constI32 0)
        (loadI32 (loadI64 (unTagClosure p) 0) offset_StgInfoTable_srt)

rtsGetDoubleFunction _ =
  runEDSL "rts_getDouble" $ do
    setReturnTypes [F64]
    p <- param I64
    emit $ loadF64 (unTagClosure p) offset_StgClosure_payload

-- rtsGetCharFunction = rtsGetIntFunction

-- generate a function which internally performs getInt, but is
-- named differently
generateRtsGetIntFunction :: BuiltinsOptions
  -> AsteriusEntitySymbol -- Name of the function
  -> AsteriusModule
generateRtsGetIntFunction _ n =
  runEDSL n $ do
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

loadI64Function _ =
  runEDSL "loadI64"  $ do
    setReturnTypes [I64]
    p <- param I64
    emit $ loadI64 p 0

printI64Function _ =
  runEDSL "print_i64" $ do
    x <- param I64
    callImport "printI64" [convertSInt64ToFloat64 x]

assertEqI64Function _ =
  runEDSL "assert_eq_i64"  $ do
    x <- param I64
    y <- param I64
    callImport
      "assertEqI64"
      [convertSInt64ToFloat64 x, convertSInt64ToFloat64 y]

printF32Function _ =
  runEDSL "print_f32" $ do
    x <- param F32
    callImport "printF32" [x]

printF64Function _ =
  runEDSL "print_f64" $ do
    x <- param F64
    callImport "printF64" [x]

strlenFunction _ =
  runEDSL "strlen"  $ do
    setReturnTypes [I64]
    [str] <- params [I64]
    len <- callImport' "__asterius_strlen" [convertUInt64ToFloat64 str] F64
    emit $ truncUFloat64ToInt64 len


debugBelch2Function _ =
  runEDSL "debugBelch2"  $ do
    [fmt, str] <- params [I64, I64]
    callImport "__asterius_debugBelch2" [convertUInt64ToFloat64 fmt, convertUInt64ToFloat64 str]

memchrFunction _ =
  runEDSL "memchr"  $ do
    setReturnTypes [I64]
    [ptr, val, num] <- params [I64, I64, I64]
    p <-
      callImport'
        "__asterius_memchr"
        (map convertUInt64ToFloat64 [ptr, val, num])
        F64
    emit $ truncUFloat64ToInt64 p

memcpyFunction _ =
  runEDSL "memcpy" $ do
    setReturnTypes [I64]
    [dst, src, n] <- params [I64, I64, I64]
    callImport "__asterius_memcpy" $ map convertUInt64ToFloat64 [dst, src, n]
    emit dst

memsetFunction _ =
  runEDSL "memset" $ do
    setReturnTypes [I64]
    [dst, c, n] <- params [I64, I64, I64]
    callImport "__asterius_memset" $ map convertUInt64ToFloat64 [dst, c, n]
    emit dst

memcmpFunction _ =
  runEDSL "memcmp" $ do
    setReturnTypes [I64]
    [ptr1, ptr2, n] <- params [I64, I64, I64]
    cres <-
      callImport'
        "__asterius_memcmp"
        (map convertUInt64ToFloat64 [ptr1, ptr2, n])
        I32
    emit $ Unary ExtendSInt32 cres

fromJSArrayBufferFunction _ =
  runEDSL "__asterius_fromJSArrayBuffer" $ do
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
  runEDSL "__asterius_toJSArrayBuffer"  $ do
    setReturnTypes [I64]
    [addr, len] <- params [I64, I64]
    r <-
      truncUFloat64ToInt64 <$>
      callImport'
        "__asterius_toJSArrayBuffer_imp"
        (map convertUInt64ToFloat64 [addr, len])
        F64
    emit r

fromJSStringFunction _ =
  runEDSL "__asterius_fromJSString"  $ do
    setReturnTypes [I64]
    [s] <- params [I64]
    addr <-
      truncUFloat64ToInt64 <$>
      callImport' "__asterius_fromJSString_imp" [convertUInt64ToFloat64 s] F64
    emit addr

fromJSArrayFunction _ =
  runEDSL "__asterius_fromJSArray"  $ do
    setReturnTypes [I64]
    [arr] <- params [I64]
    addr <-
      truncUFloat64ToInt64 <$>
      callImport' "__asterius_fromJSArray_imp" [convertUInt64ToFloat64 arr] F64
    emit addr

threadPausedFunction _ =
  runEDSL "threadPaused" $ do
    args <- params [I64, I64]
    callImport "__asterius_threadPaused" $ map convertUInt64ToFloat64 args

dirtyMutVarFunction _ =
  runEDSL "dirty_MUT_VAR" $ do
    [_, p] <- params [I64, I64]
    if'
      []
      (loadI64 p 0 `eqInt64` symbol "stg_MUT_VAR_CLEAN_info")
      (storeI64 p 0 $ symbol "stg_MUT_VAR_DIRTY_info")
      mempty

raiseExceptionHelperFunction _ =
  runEDSL "raiseExceptionHelper" $ do
    setReturnTypes [I64]
    args <- params [I64, I64, I64]
    frame_type <-
      truncUFloat64ToInt64 <$>
      callImport'
        "__asterius_raiseExceptionHelper"
        (map convertUInt64ToFloat64 args)
        F64
    emit frame_type

barfFunction _ =
  runEDSL "barf" $ do
    s <- param I64
    callImport "__asterius_barf" [convertUInt64ToFloat64 s]

-- | Note that generateRTSWrapper will treat all our numbers as signed, not
-- unsigned.   This is OK for ASCII code, since the ints we have will not be
-- larger than 2^15.  However, for larger numbers, it would "overflow", and
-- would treat large unsigned  numbers as negative signed numbers.
unicodeCBits :: [(AsteriusEntitySymbol, (FunctionImport, Function))]
unicodeCBits = map
    (\(func_sym, param_vts, ret_vts) ->
       ( AsteriusEntitySymbol func_sym
       , generateRTSWrapper "Unicode" func_sym param_vts ret_vts))
    [ ("u_gencat", [I64], [I64])
    , ("u_iswalpha", [I64], [I64])
    , ("u_iswalnum", [I64], [I64])
    , ("u_iswupper", [I64], [I64])
    , ("u_iswlower", [I64], [I64])
    , ("u_towlower", [I64], [I64])
    , ("u_towupper", [I64], [I64])
    , ("u_towtitle", [I64], [I64])
    , ("u_iswcntrl", [I64], [I64])
    , ("u_iswprint", [I64], [I64])
    ]

getProgArgvFunction _ =
  runEDSL "getProgArgv" $ do
    [argc, argv] <- params [I64, I64]
    storeI64 argc 0 $ constI64 1
    storeI64 argv 0 $ symbol "prog_argv"

suspendThreadFunction _ =
  runEDSL "suspendThread" $ do
    setReturnTypes [I64]
    [reg, _] <- params [I64, I64]
    call "threadPaused" [mainCapability, getLVal $ global CurrentTSO]
    emit reg

resumeThreadFunction _ =
  runEDSL "resumeThread" $ do
    setReturnTypes [I64]
    reg <- param I64
    emit reg

performMajorGCFunction _ =
  runEDSL "performMajorGC" $
  callImport
    "__asterius_gcRootTSO"
    [convertUInt64ToFloat64 $ getLVal $ global CurrentTSO]

performGCFunction _ = runEDSL "performGC" $ call "performMajorGC" []

localeEncodingFunction _ =
  runEDSL "localeEncoding" $ do
    setReturnTypes [I64]
    emit $ symbol "__asterius_localeEncoding"

getF64GlobalRegFunction ::
  BuiltinsOptions
  -> AsteriusEntitySymbol  -- ^ Name of the function to be created
  -> UnresolvedGlobalReg -- ^ Global register to be returned
  -> AsteriusModule -- Module containing the function
getF64GlobalRegFunction _ n gr =
  runEDSL n $ do
    setReturnTypes [F64]
    emit $ convertSInt64ToFloat64 $ getLVal $ global gr

offset_StgTSO_StgStack :: Int
offset_StgTSO_StgStack = 8 * roundup_bytes_to_words sizeof_StgTSO

-- @cheng: there is a trade-off here: Either I emit the low-level
-- store and load, or I expose a _lot more_ from the EDSL
-- to create the correct types of stores and loads I want.
-- I went with the former, but we can discuss trade-offs.
-- | Generate a wrap from the input type to the output type by invoking
-- | the correct load instruction.
-- | Since we only generate wrapping from larger types to smaller types,
-- | our output can only be {32, 16, 8} bits. However, wasm has
-- | I32 as the smallest type. So, our output is _always_ I32.
-- | invariant: output type is smaller than input type.
genWrap ::
     ValueType -- ^ Input type
  -> Int -- ^ number of bytes to load for the output type
  -> Expression
  -> Expression
genWrap ti b x =
  Block
    { name = ""
    , bodys =
        [ Store
            { bytes =
                if ti == I32
                  then 4
                  else 8
            , offset = 0
            , ptr = wrapInt64 (symbol "__asterius_i64_slot")
            , value = x
            , valueType = ti
            }
        , Load
            { signed = False
            , bytes = fromIntegral b
            , offset = 0
            , valueType = I32
            , ptr = wrapInt64 (symbol "__asterius_i64_slot")
            }
        ]
    , blockReturnTypes = [I32]
    }

-- | Whether when generate a sign extended value
data ShouldSext
  = Sext
  | NoSext
  deriving (Eq)

-- | generate a function to sign extend an input value into an output value.
-- | We perform the sign extension by storing the old value.
-- | Note that our input type is always I32. This is because we will only
-- | ever have to generate sign extension calls from GHC.W8, GHC.W16, GHC.W32,
-- | all of which are stored as I32, since wasm cannot store smaller types.
-- | So, our input will _always_ be an I32.
genExtend ::
     Int -- ^ number of bytes to load
  -> ValueType -- ^ output value type
  -> ShouldSext -- ^ whether the extend should sign-extend or not
  -> Expression
  -> Expression
genExtend b to sext x
        -- we will just use the i64 slot since it's large enough to hold all
        -- the wasm datatypes we have.
 =
  Block
    { name = ""
    , bodys =
        [ Store
            { bytes = 4
            , offset = 0
            , ptr = wrapInt64 (symbol "__asterius_i64_slot")
            , value = x
            , valueType = I32
            }
        , Load
            { signed = sext == Sext
            , bytes = fromIntegral b
            , offset = 0
            , valueType = to
            , ptr = wrapInt64 (symbol "__asterius_i64_slot")
            }
        ]
    , blockReturnTypes = [to]
    }
