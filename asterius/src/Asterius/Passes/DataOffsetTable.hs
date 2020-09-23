{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Passes.DataOffsetTable
  ( makeDataOffsetTable,
    makeMemory,
  )
where

import Asterius.EDSL
import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Bag
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.List
import Data.Monoid
import qualified Data.Set as Set
import Data.Tuple
import Foreign
import Language.Haskell.GHC.Toolkit.Constants

{-# INLINEABLE sizeofStatic #-}
sizeofStatic :: AsteriusStatic -> Word32
sizeofStatic = \case
  SymbolStatic {} -> 8
  Uninitialized x -> fromIntegral x
  Serialized buf -> fromIntegral $ BS.length buf

sizeofStatics :: AsteriusStatics -> Word32
sizeofStatics = getSum . foldMap (Sum . sizeofStatic) . asteriusStatics

{-# INLINEABLE makeDataOffsetTable #-}
makeDataOffsetTable :: AsteriusModule -> (SM.SymbolMap Word32, Word32)
makeDataOffsetTable AsteriusModule {..} =
  swap $
    SM.mapAccum
      ( \a ss -> (a + fromIntegral (fromIntegral (sizeofStatics ss) `roundup` 16), a)
      )
      0
      staticsMap

-- | Given the the offsets of symbols (for both function and static symbols),
-- create the relocation function. TODO: At the moment we create a single
-- monolithic function which is likely to overflow the maximum wasm function
-- size when the linker output is big enough. We should split it into smaller
-- parts (see https://github.com/tweag/asterius/pull/736#issuecomment-676466449).
makeWasmApplyRelocs :: Set.Set Word32 -> Set.Set Word32 -> AsteriusModule
makeWasmApplyRelocs fn_offsets ss_offsets = runEDSL "__wasm_apply_relocs" $ do
  -- Store the extended (64-bit) bases into local variables, to speed things up
  -- and keep the size of the relocation function more manageable.
  table_base <- i64Local $ extendUInt32 dynamicTableBase
  memory_base <- i64Local $ extendUInt32 dynamicMemoryBase
  for_ fn_offsets $ \off ->
    let loc = mkDynamicDataAddress off
     in storeI64 loc 0 $
          (table_base `addInt64` loadI64 loc 0)
            `orInt64` ConstI64 (functionTag `shiftL` 32)
  for_ ss_offsets $ \off ->
    let loc = mkDynamicDataAddress off
     in storeI64 loc 0 $
          (memory_base `addInt64` loadI64 loc 0)
            `orInt64` ConstI64 (dataTag `shiftL` 32)

-- | Given the offset of a static and the static itself, compute the
-- corresponding data segment and the offset of the subsequent static.
-- Furthermore, gather the offsets of symbol statics, to be used by the
-- relocation function. NOTE: we do not generate data segments for
-- uninitialized statics; we do not have to specify each segment and the
-- linear memory is zero-initialized anyway.
{-# INLINEABLE makeDynamicSegment #-}
makeDynamicSegment ::
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  (Word32, Set.Set Word32, Set.Set Word32, Builder) ->
  AsteriusStatic ->
  (Word32, Set.Set Word32, Set.Set Word32, Builder)
makeDynamicSegment fn_off_map ss_off_map (current_off, fn_meta, ss_meta, acc) static = case static of
  SymbolStatic sym o
    | Just off <- SM.lookup sym fn_off_map ->
      ( next_off,
        current_off `Set.insert` fn_meta,
        ss_meta,
        acc <> byteString (encodeStorable $ castOffsetToAddress $ fromIntegral $ off + fromIntegral o) -- To be fixed at runtime; see makeWasmApplyRelocs
      )
    | Just off <- SM.lookup sym ss_off_map ->
      ( next_off,
        fn_meta,
        current_off `Set.insert` ss_meta,
        acc <> byteString (encodeStorable $ castOffsetToAddress $ fromIntegral $ off + fromIntegral o) -- To be fixed at runtime; see makeWasmApplyRelocs
      )
    | otherwise ->
      ( next_off,
        fn_meta,
        ss_meta,
        acc <> byteString (encodeStorable invalidAddress)
      )
  Uninitialized len ->
    ( next_off,
      fn_meta,
      ss_meta,
      acc <> byteString (BS.replicate len 0)
    )
  Serialized buf ->
    ( next_off,
      fn_meta,
      ss_meta,
      acc <> byteString buf
    )
  where
    next_off = current_off + sizeofStatic static

{-# INLINEABLE castOffsetToAddress #-}
castOffsetToAddress :: Word32 -> Int64
castOffsetToAddress = fromIntegral

-- | Given the offset of a static and the static itself, compute the
-- corresponding data segment and the offset of the subsequent static. NOTE: we
-- do not generate data segments for uninitialized statics; we do not have to
-- specify each segment and the linear memory is zero-initialized anyway.
{-# INLINEABLE makeStaticSegment #-}
makeStaticSegment :: SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> Word32 -> AsteriusStatic -> (Word32, Bag DataSegment)
makeStaticSegment fn_off_map ss_off_map current_off static =
  ( current_off + sizeofStatic static,
    case static of
      SymbolStatic sym o
        | Just off <- SM.lookup sym fn_off_map ->
          unitBag
            DataSegment
              { content = encodeStorable $ mkStaticFunctionAddress (off + fromIntegral o),
                offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              }
        | Just off <- SM.lookup sym ss_off_map ->
          unitBag
            DataSegment
              { content = encodeStorable $ mkStaticDataAddress (off + fromIntegral o),
                offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              }
        | otherwise ->
          unitBag
            DataSegment
              { content = encodeStorable invalidAddress,
                offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              }
      Uninitialized {} -> emptyBag
      Serialized buf ->
        unitBag
          DataSegment
            { content = buf,
              offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
            }
  )

{-# INLINEABLE makeStaticMemory #-}
makeStaticMemory ::
  AsteriusModule ->
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  ([DataSegment], AsteriusModule) -- relocation function implementation
makeStaticMemory AsteriusModule {..} fn_off_map ss_off_map = (segs, reloc)
  where
    reloc = runEDSL "__wasm_apply_relocs" (pure ())
    segs = concat
      $ SM.elems
      $ flip SM.mapWithKey staticsMap
      $ \statics_sym AsteriusStatics {..} ->
        bagToList
          $ unionManyBags
          $ snd
          $ mapAccumL
            (makeStaticSegment fn_off_map ss_off_map)
            (ss_off_map SM.! statics_sym)
            asteriusStatics

{-# INLINEABLE makeDynamicMemory #-}
makeDynamicMemory ::
  AsteriusModule ->
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  ([DataSegment], AsteriusModule) -- relocation function implementation
makeDynamicMemory AsteriusModule {..} fn_off_map ss_off_map =
  ( [ DataSegment
        { content = toStrict $ toLazyByteString all_content, -- TODO: expensive
          offset = dynamicMemoryBase
        }
    ],
    makeWasmApplyRelocs all_fn_offs all_ss_offs
  )
  where
    (_final_offset, all_fn_offs, all_ss_offs, all_content) =
      -- TODO: Utilize final_offset.
      foldl
        ( \(_, fn_offs, ss_offs, seg_contents) (sym, AsteriusStatics {..}) ->
            foldl
              (makeDynamicSegment fn_off_map ss_off_map)
              (ss_off_map SM.! sym, fn_offs, ss_offs, seg_contents)
              asteriusStatics
        )
        (0, Set.empty, Set.empty, mempty)
        (SM.toList staticsMap)

makeMemory ::
  Bool ->
  AsteriusModule ->
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  ([DataSegment], AsteriusModule) -- relocation function implementation
makeMemory pic_is_on final_m fn_off_map ss_off_map
  | pic_is_on = makeDynamicMemory final_m fn_off_map ss_off_map
  | otherwise = makeStaticMemory final_m fn_off_map ss_off_map
