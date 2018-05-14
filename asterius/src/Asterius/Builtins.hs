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
  , stgRunSymbol
  , asteriusStaticSize
  , asteriusStaticsSize
  ) where

import Asterius.BuildInfo
import Asterius.Containers
import Asterius.Internals
import Asterius.Types
import qualified Data.ByteString.Short as SBS
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
        [ (createThreadSymbol, createThreadFunction opts)
        , (createGenThreadSymbol, createGenThreadFunction opts)
        , (createIOThreadSymbol, createIOThreadFunction opts)
        , (createStrictIOThreadSymbol, createStrictIOThreadFunction opts)
        , (allocateSymbol, allocateFunction opts)
        , (allocateMightFailSymbol, allocateMightFailFunction opts)
        , (allocatePinnedSymbol, allocatePinnedFunction opts)
        , (allocBlockSymbol, allocBlockFunction opts)
        , (allocBlockLockSymbol, allocBlockLockFunction opts)
        , (allocBlockOnNodeSymbol, allocBlockOnNodeFunction opts)
        , (allocBlockOnNodeLockSymbol, allocBlockOnNodeLockFunction opts)
        , (allocGroupSymbol, allocGroupFunction opts)
        , (allocGroupLockSymbol, allocGroupLockFunction opts)
        , (allocGroupOnNodeSymbol, allocGroupOnNodeFunction opts)
        , (allocGroupOnNodeLockSymbol, allocGroupOnNodeLockFunction opts)
        , (newCAFSymbol, newCAFFunction opts)
        , (stgRunSymbol, stgRunFunction opts)
        , (stgReturnSymbol, stgReturnFunction opts)
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
  , ("None(I64)", FunctionType {returnType = None, paramTypes = [I64]})
  ]

rtsAsteriusGlobalMap :: HashMap SBS.ShortByteString Global
rtsAsteriusGlobalMap = []

createThreadSymbol, createGenThreadSymbol, createIOThreadSymbol, createStrictIOThreadSymbol, allocateSymbol, allocateMightFailSymbol, allocatePinnedSymbol, allocBlockSymbol, allocBlockLockSymbol, allocBlockOnNodeSymbol, allocBlockOnNodeLockSymbol, allocGroupSymbol, allocGroupLockSymbol, allocGroupOnNodeSymbol, allocGroupOnNodeLockSymbol, newCAFSymbol, stgRunSymbol, stgReturnSymbol ::
     AsteriusEntitySymbol
createThreadSymbol = "createThread"

createGenThreadSymbol = "createGenThread"

createIOThreadSymbol = "createIOThread"

createStrictIOThreadSymbol = "createStrictIOThread"

allocateSymbol = "allocate"

allocateMightFailSymbol = "allocateMightFail"

allocatePinnedSymbol = "allocatePinned"

allocBlockSymbol = "allocBlock"

allocBlockLockSymbol = "allocBlock_lock"

allocBlockOnNodeSymbol = "allocBlockOnNode"

allocBlockOnNodeLockSymbol = "allocBlockOnNode_lock"

allocGroupSymbol = "allocGroup"

allocGroupLockSymbol = "allocGroup_lock"

allocGroupOnNodeSymbol = "allocGroupOnNode"

allocGroupOnNodeLockSymbol = "allocGroupOnNode_lock"

newCAFSymbol = "newCAF"

stgRunSymbol = "StgRun"

stgReturnSymbol = "StgReturn"

asteriusStaticSize :: AsteriusStatic -> Int
asteriusStaticSize s =
  case s of
    Uninitialized l -> l
    Serialized buf -> SBS.length buf
    _ -> 8

asteriusStaticsSize :: AsteriusStatics -> Int
asteriusStaticsSize ss =
  V.foldl' (\tot s -> tot + asteriusStaticSize s) 0 (asteriusStatics ss)

createThreadFunction, createGenThreadFunction, createIOThreadFunction, createStrictIOThreadFunction, allocateFunction, allocateMightFailFunction, allocatePinnedFunction, allocBlockFunction, allocBlockLockFunction, allocBlockOnNodeFunction, allocBlockOnNodeLockFunction, allocGroupFunction, allocGroupLockFunction, allocGroupOnNodeFunction, allocGroupOnNodeLockFunction, newCAFFunction, stgRunFunction, stgReturnFunction ::
     BuiltinsOptions -> Function
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
                        { target = allocateSymbol
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
                        { target = createThreadSymbol
                        , operands = [cap, stack_size_w]
                        , valueType = I64
                        }
                  }
              , saveSp 4 tso_p
              , SetLocal
                  { index = 5
                  , value =
                      Binary
                        { binaryOp = SubInt64
                        , operand0 =
                            loadWord $
                            wrapI64 $ fieldOff stack_p offset_StgStack_sp
                        , operand1 = constInt $ 8 * length closures
                        }
                  }
              , setFieldWord stack_p offset_StgStack_sp sp
              ] <>
              [ storeWord
                (fieldOff sp $ i * 8)
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
                        , operand1 = words2Bytes $ getLocalWord 1
                        }
                  }
              , If
                  { condition =
                      Binary
                        { binaryOp = GtSInt64
                        , operand0 = getLocalWord 2
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
              , UnresolvedSetGlobal
                  {unresolvedGlobalReg = Hp, value = getLocalWord 2}
              , storeWord
                  (wrapI64 $
                   fieldOff
                     (loadWord $
                      wrapI64 $
                      fieldOff
                        UnresolvedGetGlobal {unresolvedGlobalReg = BaseReg}
                        offset_StgRegTable_rCurrentAlloc)
                     offset_bdescr_free)
                  (getLocalWord 2)
              , getLocalWord 3
              ]
          , valueType = I64
          }
    }

allocateMightFailFunction _ =
  Function
    { functionTypeName = "I64(I64,I64)"
    , varTypes = []
    , body =
        Call
          { target = allocateSymbol
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
          { target = allocateSymbol
          , operands = [getLocalWord 0, getLocalWord 1]
          , valueType = I64
          }
    }

allocBlockFunction _ =
  Function
    { functionTypeName = "I64()"
    , varTypes = []
    , body =
        Call
          {target = allocGroupSymbol, operands = [ConstI64 1], valueType = I64}
    }

allocBlockLockFunction _ =
  Function
    { functionTypeName = "I64()"
    , varTypes = []
    , body = Call {target = allocBlockSymbol, operands = [], valueType = I64}
    }

allocBlockOnNodeFunction _ =
  Function
    { functionTypeName = "I64(I32)"
    , varTypes = []
    , body = Call {target = allocBlockSymbol, operands = [], valueType = I64}
    }

allocBlockOnNodeLockFunction _ =
  Function
    { functionTypeName = "I64(I32)"
    , varTypes = []
    , body =
        Call
          { target = allocBlockOnNodeSymbol
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
              [ SetLocal {index = 2, value = blocks_to_mblocks $ getLocalWord 0}
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
                                              , operand0 =
                                                  GetLocal
                                                    {index = 2, valueType = I64}
                                              , operand1 =
                                                  ConstI64 $
                                                  fromIntegral $
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
                  (offset_first_bdescr + offset_bdescr_blocks)
                  Unary
                    { unaryOp = WrapInt64
                    , operand0 =
                        Binary
                          { binaryOp = AddInt64
                          , operand0 = constInt blocks_per_mblock
                          , operand1 =
                              Binary
                                { binaryOp = MulInt64
                                , operand0 =
                                    Binary
                                      { binaryOp = SubInt64
                                      , operand0 =
                                          GetLocal {index = 2, valueType = I64}
                                      , operand1 = ConstI64 1
                                      }
                                , operand1 =
                                    ConstI64 $
                                    fromIntegral $ mblock_size `div` block_size
                                }
                          }
                    }
              , mblocks_p
              ]
          , valueType = I64
          }
    }
  where
    first_block_p = fieldOff mblocks_p offset_first_block
    mblocks_p = getLocalWord 1
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
          { target = allocGroupSymbol
          , operands = [getLocalWord 0]
          , valueType = I64
          }
    }

allocGroupOnNodeFunction _ =
  Function
    { functionTypeName = "I64(I32,I64)"
    , varTypes = []
    , body =
        Call
          { target = allocGroupSymbol
          , operands = [getLocalWord 1]
          , valueType = I64
          }
    }

allocGroupOnNodeLockFunction _ =
  Function
    { functionTypeName = "I64(I32,I64)"
    , varTypes = []
    , body =
        Call
          { target = allocGroupOnNodeSymbol
          , operands = [GetLocal {index = 0, valueType = I32}, getLocalWord 1]
          , valueType = I64
          }
    }

newCAFFunction _ =
  Function {functionTypeName = "I64(I64,I64)", varTypes = [], body = ConstI64 0}

stgRunFunction _ =
  Function
    { functionTypeName = "None(I64)"
    , varTypes = []
    , body =
        Loop
          { name = loop_lbl
          , body =
              If
                { condition =
                    Binary
                      { binaryOp = NeInt64
                      , operand0 = getLocalWord 0
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
                              {name = loop_lbl, condition = Null, value = Null}
                          ]
                      , valueType = None
                      }
                , ifFalse = Null
                }
          }
    }
  where
    loop_lbl = "StgRun_loop"

stgReturnFunction _ =
  Function {functionTypeName = "I64()", varTypes = [], body = ConstI64 0}

fieldOff :: Expression -> Int -> Expression
fieldOff p o
  | o == 0 = p
  | otherwise =
    Binary {binaryOp = AddInt64, operand0 = p, operand1 = constInt o}

setFieldWord :: Expression -> Int -> Expression -> Expression
setFieldWord p o = storeWord (wrapI64 $ fieldOff p o)

loadWord :: Expression -> Expression
loadWord p =
  Load
    {signed = False, bytes = 8, offset = 0, align = 0, valueType = I64, ptr = p}

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
