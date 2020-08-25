{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.Passes.DataSymbolTable
  ( makeDataSymbolTable,
    makeMemory,
  )
where

import Asterius.Builtins
import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Monoid
import Data.Tuple
import GHC.Int
import Language.Haskell.GHC.Toolkit.Constants

sizeofStatics :: AsteriusStatics -> Int64
sizeofStatics =
  fromIntegral
    . getSum
    . foldMap
      ( Sum . \case
          SymbolStatic {} -> 8
          Uninitialized x -> x
          Serialized buf -> BS.length buf
      )
    . asteriusStatics

{-# INLINEABLE makeDataSymbolTable #-}
makeDataSymbolTable ::
  AsteriusModule -> Int64 -> (SM.SymbolMap Int64, Int64)
makeDataSymbolTable AsteriusModule {..} l =
  swap $
    SM.mapAccum
      ( \a ss -> (a + fromIntegral (fromIntegral (sizeofStatics ss) `roundup` 16), a)
      )
      l
      staticsMap

{-# INLINEABLE makeMemory #-}
makeMemory ::
  AsteriusModule ->
  SM.SymbolMap Int64 ->
  Int64 ->
  (BinaryenIndex, [DataSegment])
makeMemory (staticsMap -> statics) sym_map last_addr = (initial_page_addr, segments)
  where
    initial_page_addr =
      fromIntegral $
        (fromIntegral (unTag last_addr) `roundup` mblock_size)
          `quot` wasmPageSize
    segments =
      SM.foldrWithKey'
        ( \statics_sym ss@AsteriusStatics {..} statics_segs ->
            fst $
              foldr'
                ( \static (static_segs, static_tail_addr) ->
                    let flush_static_segs buf =
                          ( DataSegment {content = buf, offset = static_addr} : static_segs,
                            static_addr
                          )
                          where
                            static_addr = static_tail_addr - fromIntegral (BS.length buf)
                     in case static of
                          SymbolStatic sym o ->
                            flush_static_segs
                              $ encodeStorable
                              $ case SM.lookup sym sym_map of
                                Just addr -> addr + fromIntegral o
                                _ -> invalidAddress
                          Uninitialized l ->
                            (static_segs, static_tail_addr - fromIntegral l)
                          Serialized buf -> flush_static_segs buf
                )
                ( statics_segs,
                  fromIntegral $ unTag $ sym_map SM.! statics_sym + sizeofStatics ss
                )
                asteriusStatics
        )
        []
        statics
