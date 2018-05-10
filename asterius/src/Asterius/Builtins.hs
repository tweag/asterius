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
  , fnTypeName
  , tsoSymbol
  , bdescrSymbol
  , capabilitySymbol
  , stopThreadInfoSymbol
  , stgRunSymbol
  , asteriusStaticSize
  , asteriusStaticsSize
  ) where

import Asterius.BuildInfo
import Asterius.Containers
import Asterius.Internals
import Asterius.Types
import qualified Data.ByteString.Short as SBS
import Data.List
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
  , stackSize :: Int
  }

getDefaultBuiltinsOptions :: IO BuiltinsOptions
getDefaultBuiltinsOptions =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
  GHC.runGhc (Just ghcLibDir) $ do
    _ <- GHC.getSessionDynFlags >>= GHC.setSessionDynFlags
    dflags <- GHC.getSessionDynFlags
    pure BuiltinsOptions {dflags = dflags, stackSize = 65536}

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
        [ (tsoSymbol, tsoStatics opts)
        , (stackSymbol, stackStatics opts)
        , (bdescrSymbol, bdescrStatics opts)
        , (capabilitySymbol, capabilityStatics opts)
        ]
    , functionMap =
        [ (allocateSymbol, allocateFunction opts)
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
  [ (fnTypeName, FunctionType {returnType = I64, paramTypes = []})
  , ( entityName allocateSymbol
    , FunctionType {returnType = I64, paramTypes = [I64, I64]})
  , ( entityName allocateMightFailSymbol
    , FunctionType {returnType = I64, paramTypes = [I64, I64]})
  , ( entityName allocatePinnedSymbol
    , FunctionType {returnType = I64, paramTypes = [I64, I64]})
  , ( entityName allocBlockSymbol
    , FunctionType {returnType = I64, paramTypes = []})
  , ( entityName allocBlockLockSymbol
    , FunctionType {returnType = I64, paramTypes = []})
  , ( entityName allocBlockOnNodeSymbol
    , FunctionType {returnType = I64, paramTypes = [I32]})
  , ( entityName allocBlockOnNodeLockSymbol
    , FunctionType {returnType = I64, paramTypes = [I32]})
  , ( entityName allocGroupSymbol
    , FunctionType {returnType = I64, paramTypes = [I64]})
  , ( entityName allocGroupLockSymbol
    , FunctionType {returnType = I64, paramTypes = [I64]})
  , ( entityName allocGroupOnNodeSymbol
    , FunctionType {returnType = I64, paramTypes = [I32, I64]})
  , ( entityName allocGroupOnNodeLockSymbol
    , FunctionType {returnType = I64, paramTypes = [I32, I64]})
  , ( entityName newCAFSymbol
    , FunctionType {returnType = I64, paramTypes = [I64, I64]})
  , ( entityName stgRunSymbol
    , FunctionType {returnType = None, paramTypes = [I64]})
  ]

rtsAsteriusGlobalMap :: HashMap SBS.ShortByteString Global
rtsAsteriusGlobalMap = []

fnTypeName :: SBS.ShortByteString
fnTypeName = "_asterius_FN"

tsoSymbol, tsoInfoSymbol, stackSymbol, stackInfoSymbol, bdescrSymbol, capabilitySymbol, stopThreadInfoSymbol, allocateSymbol, allocateMightFailSymbol, allocatePinnedSymbol, allocBlockSymbol, allocBlockLockSymbol, allocBlockOnNodeSymbol, allocBlockOnNodeLockSymbol, allocGroupSymbol, allocGroupLockSymbol, allocGroupOnNodeSymbol, allocGroupOnNodeLockSymbol, newCAFSymbol, stgRunSymbol, stgReturnSymbol ::
     AsteriusEntitySymbol
tsoSymbol = "_asterius_TSO"

tsoInfoSymbol = "stg_TSO_info"

stackSymbol = "_asterius_Stack"

stackInfoSymbol = "stg_STACK_info"

bdescrSymbol = "_asterius_bdescr"

capabilitySymbol = "MainCapability"

stopThreadInfoSymbol = "stg_stop_thread_info"

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

layoutStatics :: [(Int, AsteriusStatic)] -> AsteriusStatics
layoutStatics ss =
  AsteriusStatics {asteriusStatics = snd $ f (sortOn fst ss) (0, [])}
  where
    f :: [(Int, AsteriusStatic)]
      -> (Int, V.Vector AsteriusStatic)
      -> (Int, V.Vector AsteriusStatic)
    f [] r = r
    f ((x_offset, x_static):xs) (tot_len, tot_l) =
      f
        xs
        ( x_offset + asteriusStaticSize x_static
        , case x_offset - tot_len of
            0 -> tot_l <> [x_static]
            delta
              | delta > 0 -> tot_l <> [Uninitialized delta, x_static]
              | otherwise -> error "Invalid offset in layoutStatics")

tsoStatics, stackStatics, bdescrStatics, capabilityStatics ::
     BuiltinsOptions -> AsteriusStatics
tsoStatics BuiltinsOptions {..} =
  layoutStatics
    [ (0, UnresolvedStatic tsoInfoSymbol)
    , (offset_StgTSO_stackobj, UnresolvedStatic stackSymbol)
    , (offset_StgTSO_alloc_limit, Serialized (encodePrim (maxBound :: Int64)))
    ]

stackStatics BuiltinsOptions {..} =
  layoutStatics
    [ (0, UnresolvedStatic stackInfoSymbol)
    , ( offset_StgStack_sp
      , UnresolvedOffStatic stackSymbol offset_StgStack_stack)
    , (offset_StgStack_stack, Uninitialized stackSize)
    ]

bdescrStatics _ =
  layoutStatics
    [ (offset_bdescr_start, Uninitialized 8)
    , (offset_bdescr_free, Uninitialized 8)
    , (offset_bdescr_flags, Serialized (encodePrim (0 :: Word16)))
    , (offset_bdescr_blocks, Serialized (encodePrim (1 :: Word32)))
    ]

capabilityStatics _ =
  AsteriusStatics
    { asteriusStatics =
        asteriusStatics
          (layoutStatics
             [ (offset_Capability_r + o, s)
             | (o, s) <-
                 [ (offset_StgRegTable_rCurrentTSO, UnresolvedStatic tsoSymbol)
                 , ( offset_StgRegTable_rCurrentNursery
                   , UnresolvedStatic bdescrSymbol)
                 , ( offset_StgRegTable_rRet
                   , Serialized (encodePrim (0 :: Word64)))
                 ]
             ])
    }

allocateFunction, allocateMightFailFunction, allocatePinnedFunction, allocBlockFunction, allocBlockLockFunction, allocBlockOnNodeFunction, allocBlockOnNodeLockFunction, allocGroupFunction, allocGroupLockFunction, allocGroupOnNodeFunction, allocGroupOnNodeLockFunction, newCAFFunction, stgRunFunction, stgReturnFunction ::
     BuiltinsOptions -> Function
allocateFunction _ =
  Function
    { functionTypeName = entityName allocateSymbol
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
                        , operand1 =
                            Binary
                              { binaryOp = MulInt64
                              , operand0 = GetLocal {index = 1, valueType = I64}
                              , operand1 = ConstI64 8
                              }
                        }
                  }
              , If
                  { condition =
                      Binary
                        { binaryOp = GtSInt64
                        , operand0 = GetLocal {index = 2, valueType = I64}
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
                  { unresolvedGlobalReg = Hp
                  , value = GetLocal {index = 2, valueType = I64}
                  }
              , Store
                  { bytes = 8
                  , offset = 0
                  , align = 0
                  , ptr =
                      Unary
                        { unaryOp = WrapInt64
                        , operand0 =
                            Binary
                              { binaryOp = AddInt64
                              , operand0 =
                                  Load
                                    { signed = False
                                    , bytes = 8
                                    , offset = 0
                                    , align = 0
                                    , valueType = I64
                                    , ptr =
                                        Unary
                                          { unaryOp = WrapInt64
                                          , operand0 =
                                              Binary
                                                { binaryOp = AddInt64
                                                , operand0 =
                                                    UnresolvedGetGlobal
                                                      { unresolvedGlobalReg =
                                                          BaseReg
                                                      }
                                                , operand1 =
                                                    ConstI64 $
                                                    fromIntegral
                                                      offset_StgRegTable_rCurrentAlloc
                                                }
                                          }
                                    }
                              , operand1 =
                                  ConstI64 $ fromIntegral offset_bdescr_free
                              }
                        }
                  , value = GetLocal {index = 2, valueType = I64}
                  , valueType = I64
                  }
              , GetLocal {index = 3, valueType = I64}
              ]
          , valueType = I64
          }
    }

allocateMightFailFunction _ =
  Function
    { functionTypeName = entityName allocateMightFailSymbol
    , varTypes = []
    , body =
        Call
          { target = allocateSymbol
          , operands =
              [ GetLocal {index = 0, valueType = I64}
              , GetLocal {index = 1, valueType = I64}
              ]
          , valueType = I64
          }
    }

allocatePinnedFunction _ =
  Function
    { functionTypeName = entityName allocatePinnedSymbol
    , varTypes = []
    , body =
        Call
          { target = allocateSymbol
          , operands =
              [ GetLocal {index = 0, valueType = I64}
              , GetLocal {index = 1, valueType = I64}
              ]
          , valueType = I64
          }
    }

allocBlockFunction _ =
  Function
    { functionTypeName = entityName allocBlockSymbol
    , varTypes = []
    , body =
        Call
          {target = allocGroupSymbol, operands = [ConstI64 1], valueType = I64}
    }

allocBlockLockFunction _ =
  Function
    { functionTypeName = entityName allocBlockLockSymbol
    , varTypes = []
    , body = Call {target = allocBlockSymbol, operands = [], valueType = I64}
    }

allocBlockOnNodeFunction _ =
  Function
    { functionTypeName = entityName allocBlockOnNodeSymbol
    , varTypes = []
    , body = Call {target = allocBlockSymbol, operands = [], valueType = I64}
    }

allocBlockOnNodeLockFunction _ =
  Function
    { functionTypeName = entityName allocBlockOnNodeLockSymbol
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
    { functionTypeName = entityName allocGroupSymbol
    , varTypes = [I64, I64]
    , body =
        Block
          { name = ""
          , bodys =
              [ SetLocal
                  { index = 2
                  , value =
                      blocks_to_mblocks GetLocal {index = 0, valueType = I64}
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
                                        [ Unary
                                            { unaryOp = WrapInt64
                                            , operand0 =
                                                Binary
                                                  { binaryOp = MulInt64
                                                  , operand0 =
                                                      GetLocal
                                                        { index = 2
                                                        , valueType = I64
                                                        }
                                                  , operand1 =
                                                      ConstI64 $
                                                      fromIntegral $
                                                      mblock_size `div`
                                                      wasmPageSize
                                                  }
                                            }
                                        ]
                                    }
                              }
                        , operand1 = ConstI64 $ fromIntegral wasmPageSize
                        }
                  }
              , Store
                  { bytes = 8
                  , offset = 0
                  , align = 0
                  , ptr =
                      fieldOff
                        mblocks_p
                        (offset_first_bdescr + offset_bdescr_start)
                  , value = first_block_p
                  , valueType = I64
                  }
              , Store
                  { bytes = 8
                  , offset = 0
                  , align = 0
                  , ptr =
                      fieldOff
                        mblocks_p
                        (offset_first_bdescr + offset_bdescr_free)
                  , value = first_block_p
                  , valueType = I64
                  }
              , Store
                  { bytes = 8
                  , offset = 0
                  , align = 0
                  , ptr =
                      fieldOff
                        mblocks_p
                        (offset_first_bdescr + offset_bdescr_blocks)
                  , value =
                      Binary
                        { binaryOp = AddInt64
                        , operand0 = ConstI64 $ fromIntegral blocks_per_mblock
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
                  , valueType = I64
                  }
              , mblocks_p
              ]
          , valueType = I64
          }
    }
  where
    first_block_p =
      Binary
        { binaryOp = AddInt64
        , operand0 = mblocks_p
        , operand1 = ConstI64 $ fromIntegral offset_first_block
        }
    mblocks_p = GetLocal {index = 1, valueType = I64}
    mblock_round_up p =
      Binary
        { binaryOp = AndInt64
        , operand0 =
            Binary
              { binaryOp = AddInt64
              , operand0 = p
              , operand1 = ConstI64 $ fromIntegral $ mblock_size - 1
              }
        , operand1 = ConstI64 $ fromIntegral $ complement mblock_mask
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
                            , operand1 =
                                ConstI64 $ fromIntegral blocks_per_mblock
                            }
                      , operand1 = ConstI64 $ fromIntegral block_size
                      }
              , operand1 = ConstI64 $ fromIntegral mblock_size
              }
        }

allocGroupLockFunction _ =
  Function
    { functionTypeName = entityName allocGroupLockSymbol
    , varTypes = []
    , body =
        Call
          { target = allocGroupSymbol
          , operands = [GetLocal {index = 0, valueType = I64}]
          , valueType = I64
          }
    }

allocGroupOnNodeFunction _ =
  Function
    { functionTypeName = entityName allocGroupOnNodeSymbol
    , varTypes = []
    , body =
        Call
          { target = allocGroupSymbol
          , operands = [GetLocal {index = 1, valueType = I64}]
          , valueType = I64
          }
    }

allocGroupOnNodeLockFunction _ =
  Function
    { functionTypeName = entityName allocGroupOnNodeLockSymbol
    , varTypes = []
    , body =
        Call
          { target = allocGroupOnNodeSymbol
          , operands =
              [ GetLocal {index = 0, valueType = I32}
              , GetLocal {index = 1, valueType = I64}
              ]
          , valueType = I64
          }
    }

newCAFFunction _ =
  Function
    { functionTypeName = entityName newCAFSymbol
    , varTypes = []
    , body = ConstI64 0
    }

stgRunFunction _ =
  Function
    { functionTypeName = entityName stgRunSymbol
    , varTypes = []
    , body =
        Loop
          { name = loop_lbl
          , body =
              If
                { condition =
                    Binary
                      { binaryOp = NeInt64
                      , operand0 = GetLocal {index = 0, valueType = I64}
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
                                        Binary
                                          { binaryOp = SubInt32
                                          , operand0 =
                                              Unary
                                                { unaryOp = WrapInt64
                                                , operand0 =
                                                    GetLocal
                                                      { index = 0
                                                      , valueType = I64
                                                      }
                                                }
                                          , operand1 = ConstI32 1
                                          }
                                    , operands = []
                                    , typeName = fnTypeName
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
  Function {functionTypeName = fnTypeName, varTypes = [], body = ConstI64 0}

fieldOff :: Expression -> Int -> Expression
fieldOff p o =
  Unary
    { unaryOp = WrapInt64
    , operand0 =
        Binary
          { binaryOp = AddInt64
          , operand0 = p
          , operand1 = ConstI64 $ fromIntegral o
          }
    }
