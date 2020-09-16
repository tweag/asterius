{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Foreign.ExportStatic
  ( genExportStaticObj,
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

genExportStaticObj :: FFIMarshalState -> SM.SymbolMap Word32 -> Builder
genExportStaticObj FFIMarshalState {..} ss_off_map =
  "["
    <> mconcat
      ( intersperse
          ","
          $ catMaybes
            [ genExportStaticFunc k export_decl ss_off_map
              | (k, export_decl) <- SM.toList ffiExportDecls
            ]
      )
    <> "]"

genExportStaticFunc ::
  EntitySymbol ->
  FFIExportDecl ->
  SM.SymbolMap Word32 ->
  Maybe Builder
genExportStaticFunc k FFIExportDecl {ffiFunctionType = FFIFunctionType {..}, ..} ss_off_map = do
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
