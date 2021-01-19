{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Foreign.ExportStatic
  ( genExportStaticObj,
  )
where

import Asterius.Foreign.TypesTag
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Data.ByteString.Builder
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
      <> word32HexFixed off
      <> ",0x"
      <> wordHex (ffiValueTypesTag ffiParamTypes)
      <> ",0x"
      <> wordHex (ffiValueTypesTag ffiResultTypes)
      <> ","
      <> if ffiInIO then "true]" else "false]"
