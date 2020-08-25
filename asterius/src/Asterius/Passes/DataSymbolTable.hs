{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Foreign
import Language.Haskell.GHC.Toolkit.Constants

{-# INLINEABLE sizeofStatic #-}
sizeofStatic :: AsteriusStatic -> Int
sizeofStatic = \case
  SymbolStatic {} -> 8
  Uninitialized x -> x
  Serialized buf -> BS.length buf

sizeofStatics :: AsteriusStatics -> Int64
sizeofStatics =
  fromIntegral
    . getSum
    . foldMap (Sum . sizeofStatic)
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

{-# INLINEABLE makeSegment #-}
makeSegment :: Int32 -> AsteriusStatic -> (Int32, Maybe DataSegment)
makeSegment off static =
  ( off + fromIntegral (sizeofStatic static),
    case static of
      SymbolStatic {} -> Just DataSegment {content = encodeStorable off, offset = off}
      Uninitialized {} -> Nothing
      Serialized buf -> Just DataSegment {content = buf, offset = off}
  )

{-# INLINEABLE makeMemory #-}
makeMemory ::
  AsteriusModule ->
  SM.SymbolMap Int64 ->
  Int64 ->
  (BinaryenIndex, [DataSegment])
makeMemory AsteriusModule {..} sym_map last_addr = (initial_page_addr, segments)
  where
    initial_page_addr =
      fromIntegral $
        (fromIntegral (unTag last_addr) `roundup` mblock_size)
          `quot` wasmPageSize
    computeStaticAddrs statics_sym ss =
      catMaybes $ snd $ mapAccumL makeSegment initial_offset (asteriusStatics ss)
      where
        initial_offset = fromIntegral $ unTag $ sym_map SM.! statics_sym
    segments = concat $ SM.elems $ SM.mapWithKey computeStaticAddrs staticsMap
