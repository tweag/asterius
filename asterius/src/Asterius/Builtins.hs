{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins
  ( spName
  , spLimName
  , hpName
  , hpLimName
  , currentTSOName
  , currentNurseryName
  , hpAllocName
  , gcEnter1Name
  , gcFunName
  , baseRegName
  , tsoSymbol
  , stackSymbol
  , capabilitySymbol
  , baseRegSymbol
  , bdescrSymbol
  ) where

import Asterius.Types
import qualified Data.ByteString.Short as SBS

spName, spLimName, hpName, hpLimName, currentTSOName, currentNurseryName, hpAllocName, gcEnter1Name, gcFunName, baseRegName ::
     SBS.ShortByteString
spName = "_asterius_Sp"

spLimName = "_asterius_SpLim"

hpName = "_asterius_Hp"

hpLimName = "_asterius_HpLim"

currentTSOName = "_asterius_CurrentTSO"

currentNurseryName = "_asterius_CurrentNursery"

hpAllocName = "_asterius_HpAlloc"

gcEnter1Name = "_asterius_GCEnter1"

gcFunName = "_asterius_GCFun"

baseRegName = "_asterius_BaseReg"

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
