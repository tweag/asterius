{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSGen.SPT
  ( genSPT,
  )
where

import Asterius.Types.SymbolMap
import Data.ByteString.Builder
import Data.Int
import Data.List
import Data.Word

genSPT ::
  SymbolMap Int64 ->
  SymbolMap (Word64, Word64) ->
  Builder
genSPT sym_map spt_entries =
  "new Map(["
    <> mconcat
      ( intersperse
          ","
          [ "[0x"
              <> word64HexFixed w1
              <> word64HexFixed w0
              <> "n,0x"
              <> int64HexFixed (sym_map ! sym)
              <> "]"
            | (sym, (w0, w1)) <- toListESM spt_entries
          ]
      )
    <> "])"
