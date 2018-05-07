{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , getDefaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , fnTypeName
  , newCAFTypeName
  , stgRunTypeName
  , fnType
  , newCAFType
  , stgRunType
  , tsoSymbol
  , tsoInfoSymbol
  , stackSymbol
  , stackInfoSymbol
  , bdescrSymbol
  , capabilitySymbol
  , eagerBlackholeInfoSymbol
  , stopThreadInfoSymbol
  , gcEnter1Symbol
  , gcFunSymbol
  , newCAFSymbol
  , stgRunSymbol
  , stgReturnSymbol
  , tsoStatics
  , stackStatics
  , bdescrStatics
  , capabilityStatics
  , newCAFFunction
  , stgRunFunction
  , stgReturnFunction
  , asteriusStaticSize
  , asteriusStaticsSize
  ) where

import Asterius.BuildInfo
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
    pure BuiltinsOptions {dflags = dflags, stackSize = 1024576}

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
        [ (newCAFSymbol, newCAFFunction opts)
        , (stgRunSymbol, stgRunFunction opts)
        , (stgReturnSymbol, stgReturnFunction opts)
        ]
    }

fnTypeName, newCAFTypeName, stgRunTypeName :: SBS.ShortByteString
fnTypeName = "_asterius_FN"

newCAFTypeName = "_asterius_newCAF"

stgRunTypeName = "_asterius_StgRun"

fnType, newCAFType, stgRunType :: FunctionType
fnType = FunctionType {returnType = I64, paramTypes = []}

newCAFType = FunctionType {returnType = I64, paramTypes = [I64, I64]}

stgRunType = FunctionType {returnType = None, paramTypes = [I64]}

tsoSymbol, tsoInfoSymbol, stackSymbol, stackInfoSymbol, bdescrSymbol, capabilitySymbol, eagerBlackholeInfoSymbol, stopThreadInfoSymbol, gcEnter1Symbol, gcFunSymbol, newCAFSymbol, stgRunSymbol, stgReturnSymbol ::
     AsteriusEntitySymbol
tsoSymbol = "_asterius_TSO"

tsoInfoSymbol = "stg_TSO_info"

stackSymbol = "_asterius_Stack"

stackInfoSymbol = "stg_STACK_info"

bdescrSymbol = "_asterius_bdescr"

capabilitySymbol = "_asterius_Capability"

eagerBlackholeInfoSymbol = "__stg_EAGER_BLACKHOLE_info"

stopThreadInfoSymbol = "stg_stop_thread_info"

gcEnter1Symbol = "__stg_gc_enter_1"

gcFunSymbol = "__stg_gc_fun"

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

newCAFFunction, stgRunFunction, stgReturnFunction :: BuiltinsOptions -> Function
newCAFFunction _ =
  Function
    { functionTypeName = newCAFTypeName
    , varTypes = []
    , body = Return $ ConstI64 0
    }

stgRunFunction _ =
  Function
    { functionTypeName = stgRunTypeName
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
                , ifFalse = Return {value = Null}
                }
          }
    }
  where
    loop_lbl = "StgRun_loop"

stgReturnFunction _ =
  Function
    {functionTypeName = fnTypeName, varTypes = [], body = Return $ ConstI64 0}
