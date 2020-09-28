{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSGen.SPT
  ( genSPT,
  )
where

import qualified Asterius.Types.SymbolMap as SM
import Data.ByteString.Builder
import Data.List
import Data.Word

genSPT ::
  SM.SymbolMap Word32 ->
  SM.SymbolMap (Word64, Word64) ->
  Builder
genSPT ss_off_map spt_entries =
  "new Map(["
    <> mconcat
      ( intersperse
          ","
          [ "[0x"
              <> word64HexFixed w1
              <> word64HexFixed w0
              <> "n,0x"
              <> word32HexFixed (ss_off_map SM.! sym)
              <> "]"
            | (sym, (w0, w1)) <- SM.toList spt_entries
          ]
      )
    <> "])"
