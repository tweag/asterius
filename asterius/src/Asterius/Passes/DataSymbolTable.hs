{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.DataSymbolTable
  ( makeDataSymbolTable,
    makeMemory,
  )
where

import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import Asterius.Types.EntitySymbolMap
import Data.Bits
import qualified Data.ByteString as BS
import Data.Foldable
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
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

unTag :: Int64 -> Int64
unTag = (.&. 0xFFFFFFFF)

{-# INLINEABLE makeDataSymbolTable #-}
makeDataSymbolTable ::
  AsteriusModule -> Int64 -> (EntitySymbolMap Int64, Int64)
makeDataSymbolTable AsteriusModule {..} l =
  swap $
    mapAccumESM
      ( \a ss -> (a + fromIntegral (fromIntegral (sizeofStatics ss) `roundup` 16), a)
      )
      l
      staticsMap

{-# INLINEABLE makeMemory #-}
makeMemory ::
  AsteriusModule ->
  EntitySymbolMap Int64 ->
  Int64 ->
  (BinaryenIndex, [DataSegment])
makeMemory AsteriusModule {..} sym_map last_addr =
  ( fromIntegral $
      (fromIntegral (unTag last_addr) `roundup` mblock_size)
        `quot` 65536,
    foldrWithKeyESM
      ( \statics_sym ss@AsteriusStatics {..} statics_segs ->
          fst $
            foldr'
              ( \static (static_segs, static_tail_addr) ->
                  let flush_static_segs buf =
                        ( case static_segs of
                            DataSegment {..} : static_segs'
                              | offset == static_tail_addr ->
                                DataSegment {content = buf <> content, offset = static_addr}
                                  : static_segs'
                            _ ->
                              DataSegment {content = buf, offset = static_addr}
                                : static_segs,
                          static_addr
                        )
                        where
                          static_addr = static_tail_addr - fromIntegral (BS.length buf)
                   in case static of
                        SymbolStatic sym o ->
                          flush_static_segs
                            $ encodeStorable
                            $ case lookupESM sym sym_map of
                              Just addr -> addr + fromIntegral o
                              _ -> invalidAddress
                        Uninitialized l ->
                          (static_segs, static_tail_addr - fromIntegral l)
                        Serialized buf -> flush_static_segs buf
              )
              ( statics_segs,
                fromIntegral $ unTag $ sym_map ! statics_sym + sizeofStatics ss
              )
              asteriusStatics
      )
      []
      staticsMap
  )
