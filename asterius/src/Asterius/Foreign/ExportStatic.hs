{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Foreign.ExportStatic
  ( genFunctionsExportStaticObj,
    genStaticsExportStaticObj,
    encodeTys,
  )
where

import Asterius.Foreign.SupportedTypes
import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Data.Bits
import Data.ByteString.Builder
import Data.Foldable
import Data.Int
import Data.List
import Data.Maybe
import Data.Word

-- TODO: Aren't these supposed to be only functions?

genFunctionsExportStaticObj :: FFIMarshalState -> SM.SymbolMap Word32 -> Builder
genFunctionsExportStaticObj FFIMarshalState {..} fn_off_map =
  "["
    <> mconcat
      ( intersperse
          ","
          $ catMaybes
            [ genFunctionsExportStaticFunc k export_decl fn_off_map
              | (k, export_decl) <- SM.toList ffiExportDecls
            ]
      )
    <> "]"

genStaticsExportStaticObj :: FFIMarshalState -> SM.SymbolMap Word32 -> Builder
genStaticsExportStaticObj FFIMarshalState {..} ss_off_map =
  "["
    <> mconcat
      ( intersperse
          ","
          $ catMaybes
            [ genStaticsExportStaticFunc k export_decl ss_off_map
              | (k, export_decl) <- SM.toList ffiExportDecls
            ]
      )
    <> "]"

genFunctionsExportStaticFunc ::
  EntitySymbol ->
  FFIExportDecl ->
  SM.SymbolMap Word32 ->
  Maybe Builder
genFunctionsExportStaticFunc k FFIExportDecl {ffiFunctionType = FFIFunctionType {..}, ..} fn_off_map = do
  off <- SM.lookup ffiExportClosure fn_off_map
  pure $
    "[\""
      <> byteString (entityName k)
      <> "\",0x"
      <> int64HexFixed (mkFunctionAddress off)
      <> ",0x"
      <> int64HexFixed (encodeTys ffiParamTypes)
      <> ",0x"
      <> int64HexFixed (encodeTys ffiResultTypes)
      <> ","
      <> if ffiInIO then "true]" else "false]"

genStaticsExportStaticFunc ::
  EntitySymbol ->
  FFIExportDecl ->
  SM.SymbolMap Word32 ->
  Maybe Builder
genStaticsExportStaticFunc k FFIExportDecl {ffiFunctionType = FFIFunctionType {..}, ..} ss_off_map = do
  off <- SM.lookup ffiExportClosure ss_off_map
  pure $
    "[\""
      <> byteString (entityName k)
      <> "\",0x"
      <> int64HexFixed (mkDataAddress off)
      <> ",0x"
      <> int64HexFixed (encodeTys ffiParamTypes)
      <> ",0x"
      <> int64HexFixed (encodeTys ffiResultTypes)
      <> ","
      <> if ffiInIO then "true]" else "false]"

encodeTys :: [FFIValueType] -> Int64
encodeTys = foldr' (\vt acc -> (acc `shiftL` 5) .|. encodeTy vt) 0

encodeTy :: FFIValueType -> Int64
encodeTy vt =
  case findIndex (\vt' -> hsTyCon vt == hsTyCon vt') ffiBoxedValueTypeList of
    Just i -> fromIntegral i + 1
    _ -> error $ "Asterius.Foreign.ExportStatic: cannot encode " <> show vt
