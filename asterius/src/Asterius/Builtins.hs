{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins
  ( rtsAsteriusModuleSymbol
  , fnTypeName
  , fnType
  , globalRegName
  , tsoSymbol
  , stackSymbol
  , capabilitySymbol
  , baseRegSymbol
  , bdescrSymbol
  , tsoStatics
  , stackStatics
  , capabilityStatics
  , baseRegStatics
  , bdescrStatics
  ) where

import Asterius.Types
import qualified Data.ByteString.Short as SBS
import Data.String
import qualified Data.Vector as V
import qualified GhcPlugins as GHC

rtsAsteriusModuleSymbol :: AsteriusModuleSymbol
rtsAsteriusModuleSymbol =
  AsteriusModuleSymbol
    { unitId = SBS.toShort $ GHC.fs_bs $ GHC.unitIdFS GHC.rtsUnitId
    , moduleName = ["Asterius"]
    }

fnTypeName :: SBS.ShortByteString
fnTypeName = "_asterius_FN"

fnType :: FunctionType
fnType = FunctionType {returnType = I64, paramTypes = []}

globalRegName :: UnresolvedGlobalReg -> SBS.ShortByteString
globalRegName gr =
  case gr of
    VanillaReg i -> fromString $ "_asterius_R" <> show i
    FloatReg i -> fromString $ "_asterius_F" <> show i
    DoubleReg i -> fromString $ "_asterius_D" <> show i
    Sp -> "_asterius_Sp"
    SpLim -> "_asterius_SpLim"
    Hp -> "_asterius_Hp"
    HpLim -> "_asterius_HpLim"
    CurrentTSO -> "_asterius_CurrentTSO"
    CurrentNursery -> "_asterius_CurrentNursery"
    HpAlloc -> "_asterius_HpAlloc"
    GCEnter1 -> "_asterius_GCEnter1"
    GCFun -> "_asterius_GCFun"
    BaseReg -> "_asterius_BaseReg"

tsoSymbol, stackSymbol, capabilitySymbol, baseRegSymbol, bdescrSymbol ::
     AsteriusEntitySymbol
tsoSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_TSO"}

stackSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_Stack"}

capabilitySymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_Capability"}

baseRegSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_BaseReg"}

bdescrSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_bdescr"}

asteriusStaticSize :: AsteriusStatic -> Int
asteriusStaticSize s =
  case s of
    Uninitialized l -> l
    Serialized buf -> SBS.length buf
    _ -> 8

layoutStatics :: [(Int, AsteriusStatic)] -> AsteriusStatics
layoutStatics ss = AsteriusStatics {asteriusStatics = snd $ f ss (0, [])}
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
            delta -> tot_l <> [Uninitialized delta, x_static])

tsoStatics, stackStatics, capabilityStatics, baseRegStatics, bdescrStatics ::
     GHC.DynFlags -> AsteriusStatics
tsoStatics dflags =
  layoutStatics
    [ (GHC.oFFSET_StgTSO_stackobj dflags, UnresolvedStatic stackSymbol)
    , (GHC.oFFSET_StgTSO_alloc_limit dflags, undefined)
    ]

stackStatics _ = layoutStatics []

capabilityStatics dflags =
  layoutStatics
    [(GHC.oFFSET_Capability_r dflags, UnresolvedStatic baseRegSymbol)]

baseRegStatics _ = layoutStatics []

bdescrStatics dflags =
  layoutStatics
    [ (GHC.oFFSET_bdescr_start dflags, undefined)
    , (GHC.oFFSET_bdescr_free dflags, undefined)
    , (GHC.oFFSET_bdescr_flags dflags, undefined)
    , (GHC.oFFSET_bdescr_blocks dflags, undefined)
    ]
