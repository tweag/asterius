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
{-# INLINEABLE makeSegment #-}
makeSegment ::
  Bool ->
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  (Word32, Set.Set Word32, Set.Set Word32) ->
  AsteriusStatic ->
  ((Word32, Set.Set Word32, Set.Set Word32), Bag DataSegment)
makeSegment pic_is_on fn_off_map ss_off_map (current_off, fn_meta, ss_meta) static = case static of
  SymbolStatic sym o
    | Just off <- SM.lookup sym fn_off_map ->
      let (address, seg_off)
            | pic_is_on =
              ( fromIntegral $ off + fromIntegral o, -- To be fixed at runtime; see makeWasmApplyRelocs
                ConstI32 $ fromIntegral $ defaultMemoryBase + current_off -- TODO: make dynamic
              )
            | otherwise =
              ( mkStaticFunctionAddress $ off + fromIntegral o,
                ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              )
       in ( (next_off, current_off `Set.insert` fn_meta, ss_meta),
            unitBag
              DataSegment
                { content = encodeStorable address,
                  offset = seg_off
                }
          )
    | Just off <- SM.lookup sym ss_off_map ->
      let (address, seg_off)
            | pic_is_on =
              ( fromIntegral $ off + fromIntegral o, -- To be fixed at runtime; see makeWasmApplyRelocs
                ConstI32 $ fromIntegral $ defaultMemoryBase + current_off -- TODO: make dynamic.
              )
            | otherwise =
              ( mkStaticDataAddress $ off + fromIntegral o,
                ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              )
       in ( (next_off, fn_meta, current_off `Set.insert` ss_meta),
            unitBag
              DataSegment
                { content = encodeStorable address,
                  offset = seg_off
                }
          )
    | otherwise ->
      let (address, seg_off)
            | pic_is_on =
              ( invalidAddress,
                ConstI32 $ fromIntegral $ defaultMemoryBase + current_off -- TODO: make dynamic.
              )
            | otherwise =
              ( invalidAddress,
                ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              )
       in ( (next_off, fn_meta, ss_meta),
            unitBag
              DataSegment
                { content = encodeStorable address,
                  offset = seg_off
                }
          )
  Uninitialized {} ->
    ( (next_off, fn_meta, ss_meta),
      emptyBag
    )
  Serialized buf ->
    ( (next_off, fn_meta, ss_meta),
      unitBag
        DataSegment
          { content = buf,
            offset =
              if pic_is_on
                then ConstI32 $ fromIntegral $ defaultMemoryBase + current_off -- TODO: make dynamic.
                else ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
          }
    )
  where
    next_off = current_off + sizeofStatic static

{-# INLINEABLE makeMemory #-}
makeMemory ::
  Bool ->
  AsteriusModule ->
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  ([DataSegment], AsteriusModule) -- relocation function implementation
makeMemory pic_is_on AsteriusModule {..} fn_off_map ss_off_map
  | pic_is_on = (segments, reloc)
  | otherwise = (segments, mempty)
  where
    segments = bagToList $ unionManyBags [segs | (segs, _, _) <- all_data]
    reloc =
      makeWasmApplyRelocs
        (Set.unions [fn_offs | (_, fn_offs, _) <- all_data])
        (Set.unions [ss_offs | (_, _, ss_offs) <- all_data])
    all_data :: [(Bag DataSegment, Set.Set Word32, Set.Set Word32)]
    all_data =
      SM.elems
        $ flip SM.mapWithKey staticsMap
        $ \statics_sym AsteriusStatics {..} ->
          case mapAccumL
            (makeSegment pic_is_on fn_off_map ss_off_map)
            (ss_off_map SM.! statics_sym, Set.empty, Set.empty)
            asteriusStatics of
            ((_, fn_offs, ss_offs), mb_segs) -> (unionManyBags mb_segs, fn_offs, ss_offs)
