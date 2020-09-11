{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Foreign.ExportStatic
  ( genExportStaticObj,
    encodeTys,
  )
where

import Asterius.Foreign.SupportedTypes
import Asterius.Internals.SafeFromIntegral
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Data.Bits
import Data.ByteString.Builder
import Data.Foldable
import Data.Int
import Data.List
import Data.Maybe

genExportStaticObj :: FFIMarshalState -> SM.SymbolMap Int64 -> Builder
genExportStaticObj FFIMarshalState {..} sym_map =
  "["
    <> mconcat
      ( intersperse
          ","
          $ catMaybes
            [ genExportStaticFunc k export_decl sym_map
              | (k, export_decl) <- SM.toList ffiExportDecls
            ]
      )
    <> "]"

genExportStaticFunc ::
  EntitySymbol ->
  FFIExportDecl ->
  SM.SymbolMap Int64 ->
  Maybe Builder
genExportStaticFunc k FFIExportDecl {ffiFunctionType = FFIFunctionType {..}, ..} sym_map = do
  address <- SM.lookup ffiExportClosure sym_map
  pure $
    "[\""
      <> byteString (entityName k)
      <> "\",0x"
      <> int64HexFixed address
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
    Just i -> safeFromIntegral i + 1
    _ -> error $ "Asterius.Foreign.ExportStatic: cannot encode " <> show vt
