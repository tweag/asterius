{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSGen.SPT
  ( genSPT,
  )
where

import qualified Asterius.Types.SymbolMap as SM
import Data.ByteString.Builder
import Data.Int
import Data.List
import Data.Word

genSPT ::
  SM.SymbolMap Int64 ->
  SM.SymbolMap (Word64, Word64) ->
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
              <> int64HexFixed (sym_map SM.! sym)
              <> "]"
            | (sym, (w0, w1)) <- SM.toList spt_entries
          ]
      )
    <> "])"
