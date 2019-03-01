{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.DataSymbolTable
  ( makeDataSymbolTable
  , makeMemory
  ) where

import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import Data.Bits
import qualified Data.ByteString.Short as SBS
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Tuple
import GHC.Int
import Language.Haskell.GHC.Toolkit.Constants

sizeofStatics :: AsteriusStatics -> Int64
sizeofStatics =
  fromIntegral .
  getSum .
  foldMap'
    (Sum . \case
       SymbolStatic {} -> 8
       Uninitialized x -> x
       Serialized buf -> SBS.length buf) .
  asteriusStatics

unTag :: Int64 -> Int64
unTag = (.&. 0xFFFFFFFF)

makeDataSymbolTable ::
     AsteriusModule -> Int64 -> (Map AsteriusEntitySymbol Int64, Int64)
makeDataSymbolTable AsteriusModule {..} l =
  swap $
  Map.mapAccum
    (\a ss ->
       (a + fromIntegral (fromIntegral (sizeofStatics ss) `roundup` 16), a))
    l
    staticsMap

makeMemory ::
     AsteriusModule -> Map AsteriusEntitySymbol Int64 -> Int64 -> Memory
makeMemory AsteriusModule {..} sym_map last_addr =
  Memory
    { initialPages =
        fromIntegral $
        (fromIntegral (unTag last_addr) `roundup` mblock_size) `quot` 65536
    , memoryExportName = "memory"
    , dataSegments =
        Map.foldrWithKey'
          (\statics_sym AsteriusStatics {..} statics_segs ->
             fst $
             foldl'
               (\(static_segs, static_addr) static ->
                  case static of
                    SymbolStatic sym o ->
                      ( DataSegment
                          { content =
                              encodeStorable $
                              case Map.lookup sym sym_map of
                                Just addr -> addr + fromIntegral o
                                _ -> invalidSymbol
                          , offset = static_addr
                          } :
                        static_segs
                      , static_addr + 8)
                    Uninitialized l ->
                      (static_segs, static_addr + fromIntegral l)
                    Serialized buf ->
                      ( DataSegment {content = buf, offset = static_addr} :
                        static_segs
                      , static_addr + fromIntegral (SBS.length buf)))
               (statics_segs, fromIntegral $ unTag $ sym_map ! statics_sym)
               asteriusStatics)
          []
          staticsMap
    }
