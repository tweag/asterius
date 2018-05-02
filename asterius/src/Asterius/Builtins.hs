{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

#include "DerivedConstants.h"

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , getDefaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , fnTypeName
  , barfTypeName
  , fnType
  , barfType
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
  , stgRunSymbol
  , stgReturnSymbol
  , barfSymbol
  , tsoStatics
  , stackStatics
  , bdescrStatics
  , capabilityStatics
  , stgReturnFunction
  , barfFunction
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
        [ (stgReturnSymbol, stgReturnFunction opts)
        , (barfSymbol, barfFunction opts)
        ]
    }

fnTypeName, barfTypeName :: SBS.ShortByteString
fnTypeName = "_asterius_FN"

barfTypeName = "_asterius_barf"

fnType, barfType :: FunctionType
fnType = FunctionType {returnType = I64, paramTypes = []}

barfType = FunctionType {returnType = None, paramTypes = [I64]}

tsoSymbol, tsoInfoSymbol, stackSymbol, stackInfoSymbol, bdescrSymbol, capabilitySymbol, eagerBlackholeInfoSymbol, stopThreadInfoSymbol, gcEnter1Symbol, gcFunSymbol, stgRunSymbol, stgReturnSymbol, barfSymbol ::
     AsteriusEntitySymbol
tsoSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_TSO"}

tsoInfoSymbol =
  AsteriusEntitySymbol {entityKind = StaticsEntity, entityName = "stg_TSO_info"}

stackSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_Stack"}

stackInfoSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "stg_STACK_info"}

bdescrSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_bdescr"}

capabilitySymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_Capability"}

eagerBlackholeInfoSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "__stg_EAGER_BLACKHOLE_info"}

stopThreadInfoSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "stg_stop_thread_info"}

gcEnter1Symbol =
  AsteriusEntitySymbol
    {entityKind = FunctionEntity, entityName = "__stg_gc_enter_1"}

gcFunSymbol =
  AsteriusEntitySymbol
    {entityKind = FunctionEntity, entityName = "__stg_gc_fun"}

stgRunSymbol =
  AsteriusEntitySymbol {entityKind = FunctionEntity, entityName = "StgRun"}

stgReturnSymbol =
  AsteriusEntitySymbol {entityKind = FunctionEntity, entityName = "StgReturn"}

barfSymbol =
  AsteriusEntitySymbol {entityKind = FunctionEntity, entityName = "barf"}

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
    , (OFFSET_StgTSO_stackobj, UnresolvedStatic stackSymbol)
    , (OFFSET_StgTSO_alloc_limit, Serialized (encodePrim (maxBound :: Int64)))
    ]

stackStatics BuiltinsOptions {..} =
  layoutStatics
    [ (0, UnresolvedStatic stackInfoSymbol)
    , ( OFFSET_StgStack_sp
      , UnresolvedOffStatic stackSymbol OFFSET_StgStack_stack)
    , (OFFSET_StgStack_stack, Uninitialized stackSize)
    ]

bdescrStatics _ =
  layoutStatics
    [ (OFFSET_bdescr_start, Uninitialized 8)
    , (OFFSET_bdescr_free, Uninitialized 8)
    , (OFFSET_bdescr_flags, Serialized (encodePrim (0 :: Word16)))
    , (OFFSET_bdescr_blocks, Serialized (encodePrim (1 :: Word32)))
    ]

capabilityStatics _ =
  AsteriusStatics
    { asteriusStatics =
        asteriusStatics
          (layoutStatics $
           [ (OFFSET_Capability_r + o, s)
           | (o, s) <-
               [ (OFFSET_StgRegTable_rCurrentTSO, UnresolvedStatic tsoSymbol)
               , ( OFFSET_StgRegTable_rCurrentNursery
                 , UnresolvedStatic bdescrSymbol)
               , ( OFFSET_StgRegTable_rRet
                 , Serialized (encodePrim (0 :: Word64)))
               ]
           ] <>
           [(OFFSET_Capability_sparks, Serialized (encodePrim (0 :: Word64)))])
    }

stgReturnFunction :: BuiltinsOptions -> Function
stgReturnFunction _ =
  Function
    {functionTypeName = fnTypeName, varTypes = [], body = Return $ ConstI64 0}

barfFunction :: BuiltinsOptions -> Function
barfFunction _ =
  Function {functionTypeName = barfTypeName, varTypes = [], body = Unreachable}
